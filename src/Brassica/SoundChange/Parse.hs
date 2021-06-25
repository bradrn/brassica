{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Brassica.SoundChange.Parse
    ( ParseLexeme
    , parseRule
    , parseSoundChanges
    , Component(..)
    , getWords
    , unsafeCastComponent
    , zipWithComponents
    , tokeniseWords
    , detokeniseWords
    , detokeniseWords'
    , Config(..)
      -- * Re-exports
    , errorBundlePretty
    ) where

import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.Function (on)
import Data.List (sortBy, transpose)
import Data.Maybe (isNothing, isJust, fromJust, mapMaybe)
import Data.Ord (Down(..))
import Data.Void (Void)

import Control.Applicative.Permutations
import Control.Monad.State
import qualified Data.Map.Strict as M

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Brassica.SoundChange.Types
import qualified Brassica.SoundChange.Category as C

newtype Config = Config
    { categories :: C.Categories Grapheme
    }
type Parser = ParsecT Void String (State Config)

class ParseLexeme (a :: LexemeType) where
    parseLexeme :: Parser (Lexeme a)
    parseCategoryElement :: Parser (CategoryElement a)

-- space consumer which does not match newlines
sc :: Parser ()
sc = L.space space1' (L.skipLineComment ";") empty
  where
    -- adapted from megaparsec source: like 'space1', but does not
    -- consume newlines (which are important for rule separation)
    space1' = void $ takeWhile1P (Just "white space") ((&&) <$> isSpace <*> (/='\n'))

-- space consumer which matches newlines
scn :: Parser ()
scn = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keyChars :: [Char]
keyChars = "#[](){}>\\→/_^%~*"

parseGrapheme :: Parser (Grapheme, Bool)
parseGrapheme = lexeme $ (,) <$> takeWhile1P Nothing (not . ((||) <$> isSpace <*> (`elem` keyChars))) <*> (isJust <$> optional (char '~'))

parseGrapheme' :: Parser Grapheme
parseGrapheme' = lexeme $ takeWhile1P Nothing (not . ((||) <$> isSpace <*> (=='=')))

data CategoryModification a
    = Union     (CategoryElement a)
    | Intersect (CategoryElement a)
    | Subtract  (CategoryElement a)

parseGraphemeOrCategory :: ParseLexeme a => Parser (Lexeme a)
parseGraphemeOrCategory = do
    (g, isntCat) <- parseGrapheme
    if isntCat
        then return $ Grapheme g
        else do
            cats <- gets categories
            return $ case C.lookup g cats of
                Nothing -> Grapheme g
                Just c  -> Category $ C.bake $ GraphemeEl <$> c

parseCategory :: ParseLexeme a => Parser (Lexeme a)
parseCategory = do
    mods <- symbol "[" *> someTill parseCategoryModification (symbol "]")
    cats <- gets categories
    return $ Category $ C.bake $
        C.expand (C.mapCategories GraphemeEl cats) (toCategory mods)

parseCategoryStandalone :: Parser (Grapheme, C.Category 'C.Expanded Grapheme)
parseCategoryStandalone = do
    g <- parseGrapheme'
    _ <- symbol "="
    -- Use Target here because it only allows graphemes, not boundaries
    mods <- some (parseCategoryModification @'Target)
    cats <- gets categories
    return (g, C.expand cats $ toGrapheme <$> toCategory mods)

toGrapheme :: CategoryElement 'Target -> Grapheme
toGrapheme (GraphemeEl g) = g

categoriesDeclParse :: Parser CategoriesDecl
categoriesDeclParse = do
    overwrite <- isJust <$> optional (symbol "new")
    when overwrite $ put $ Config M.empty
    _ <- symbol "categories" <* scn
    -- parse category declarations, adding to the set of known
    -- categories as each is parsed
    _ <- some $ parseFeature <|> parseCategoryDecl
    _ <- symbol "end" <* scn
    Config catsNew <- get
    return $ CategoriesDecl (C.values catsNew)
  where
    parseFeature = do
        _ <- symbol "feature"
        namePlain <- optional $ try $ parseGrapheme' <* symbol "="
        modsPlain <- some (parseCategoryModification @'Target)
        cats <- gets categories
        let plainCat = C.expand cats $ toGrapheme <$> toCategory modsPlain
            plain = C.bake plainCat
        modifiedCats <- some (symbol "/" *> parseCategoryStandalone) <* scn
        let modified = C.bake . snd <$> modifiedCats
            syns = zipWith (\a b -> (a, C.UnionOf [C.Node a, C.categorise b])) plain $ transpose modified
        modify $ \(Config cs) -> Config $ M.unions
                [ M.fromList syns
                , M.fromList modifiedCats
                , case namePlain of
                      Nothing -> M.empty
                      Just n -> M.singleton n plainCat
                , cs
                ]
    parseCategoryDecl = do
        (k, c) <- try parseCategoryStandalone <* scn
        modify $ \(Config cs) -> Config (M.insert k c cs)

parseCategoryModification :: ParseLexeme a => Parser (CategoryModification a)
parseCategoryModification = parsePrefix <*> parseCategoryElement
  where
    parsePrefix =
        (Intersect <$ char '+')
        <|> (Subtract <$ char '-')
        <|> pure Union

toCategory :: [CategoryModification a] -> C.Category 'C.Unexpanded (CategoryElement a)
toCategory = go C.Empty
  where
    go c [] = c
    go c (Union e    :es) = go (C.UnionOf  [c, C.Node e]) es
    go c (Intersect e:es) = go (C.Intersect c (C.Node e)) es
    go c (Subtract e :es) = go (C.Subtract  c (C.Node e)) es

parseOptional :: ParseLexeme a => Parser (Lexeme a)
parseOptional = Optional <$> between (symbol "(") (symbol ")") (some parseLexeme)

parseGeminate :: Parser (Lexeme a)
parseGeminate = Geminate <$ symbol ">"

parseMetathesis :: Parser (Lexeme 'Replacement)
parseMetathesis = Metathesis <$ symbol "\\"

parseWildcard :: (ParseLexeme a, OneOf a 'Target 'Env) => Parser (Lexeme a)
parseWildcard = Wildcard <$> (symbol "^" *> parseLexeme)

parseBoundary :: Parser ()
parseBoundary = () <$ symbol "#"

parseDiscard :: Parser (Lexeme 'Replacement)
parseDiscard = Discard <$ symbol "~"

parseKleene :: OneOf a 'Target 'Env => Lexeme a -> Parser (Lexeme a)
parseKleene l = (Kleene l <$ symbol "*") <|> pure l

instance ParseLexeme 'Target where
    parseLexeme = asum
        [ parseCategory
        , parseOptional
        , parseGeminate
        , parseWildcard
        , parseGraphemeOrCategory
        ] >>= parseKleene
    parseCategoryElement = GraphemeEl . fst <$> parseGrapheme

instance ParseLexeme 'Replacement where
    parseLexeme = asum
        [ parseCategory
        , parseOptional
        , parseMetathesis
        , parseDiscard
        , parseGeminate
        , parseGraphemeOrCategory
        ]
    parseCategoryElement = GraphemeEl . fst <$> parseGrapheme

instance ParseLexeme 'Env where
    parseLexeme = asum
        [ parseCategory
        , Boundary <$ parseBoundary
        , parseOptional
        , parseGeminate
        , parseWildcard
        , parseGraphemeOrCategory
        ] >>= parseKleene
    parseCategoryElement = asum
        [ BoundaryEl <$ parseBoundary
        , GraphemeEl . fst <$> parseGrapheme
        ]

parseLexemes :: ParseLexeme a => Parser [Lexeme a]
parseLexemes = many parseLexeme

parseFlags :: Parser Flags
parseFlags = runPermutation $ Flags
    <$> toPermutation (isNothing <$> optional (symbol "-x"))
    <*> toPermutationWithDefault LTR ((LTR <$ symbol "-ltr") <|> (RTL <$ symbol "-rtl"))
    <*> toPermutation (isJust <$> optional (symbol "-1"))

ruleParser :: Parser Rule
ruleParser = do
    -- This is an inlined version of 'match' from @megaparsec@;
    -- 'match' itself would be tricky to use here, since it would need
    -- to wrap multiple parsers rather than just one
    o <- getOffset
    s <- getInput

    flags <- parseFlags
    target <- parseLexemes
    _ <- lexeme $ oneOf "/→"
    replacement <- parseLexemes

    let parseEnvironment = do
            _ <- symbol "/"
            env1 <- parseLexemes
            _ <- symbol "_"
            env2 <- parseLexemes
            exception <- optional $ (,) <$> (symbol "/" *> parseLexemes) <* symbol "_" <*> parseLexemes
            return (env1, env2, exception)

    (env1, env2, exception) <- parseEnvironment <|> pure ([], [], Nothing)

    _ <- optional scn   -- consume newline after rule if present

    o' <- getOffset
    let plaintext = (fst . fromJust) (takeN_ (o' - o) s)
    return Rule{environment=(env1,env2), ..}

-- | Parse a 'String' to get a 'Rule'. Returns 'Nothing' if the input
-- string is malformed.
parseRule :: String -> Either (ParseErrorBundle String Void) Rule
parseRule s = flip evalState (Config M.empty) $ runParserT (scn *> ruleParser <* eof) "" s

-- | Parse a list of 'SoundChanges'.
parseSoundChanges :: String -> Either (ParseErrorBundle String Void) SoundChanges
parseSoundChanges s = flip evalState (Config M.empty) $ runParserT (scn *> parser <* eof) "" s
  where
    parser = many $
        CategoriesDeclS <$> categoriesDeclParse
        <|> RuleS <$> ruleParser

-- | Represents a component of a parsed input string. The type
-- variable will usually be something like '[Grapheme]', though it
-- depends on the type of words you’re parsing.
data Component a = Word a | Whitespace String | Gloss String
    deriving (Eq, Show, Functor)

getWords :: [Component a] -> [a]
getWords = mapMaybe $ \case
    Word a -> Just a
    _ -> Nothing

unsafeCastComponent :: Component a -> Component b
unsafeCastComponent (Word _) = error "unsafeCastComponent: attempted to cast a word!"
unsafeCastComponent (Whitespace s) = Whitespace s
unsafeCastComponent (Gloss s) = Gloss s

tokeniseWords :: [Grapheme] -> String -> Either (ParseErrorBundle String Void) [Component [Grapheme]]
tokeniseWords (sortBy (compare `on` Down . length) -> gs) =
    parse @Void
        (many $
            (Whitespace <$> takeWhile1P Nothing isSpace) <|>
            (Gloss <$> (char '[' *> takeWhileP Nothing (/=']') <* char ']')) <|>
            (Word <$> parseWord))
        ""
  where
    parseWord = some $ choice (chunk <$> gs) <|> (pure <$> satisfy (not . isSpace))

detokeniseWords :: [Component [Grapheme]] -> String
detokeniseWords = detokeniseWords' concat

detokeniseWords' :: (a -> String) -> [Component a] -> String
detokeniseWords' f = concatMap $ \case
    Word gs -> f gs
    Whitespace w -> w
    Gloss g -> '[':(g ++ "]")

zipWithComponents :: [Component a] -> [Component b] -> b -> (a -> b -> c) -> [Component c]
zipWithComponents []             _            _  _ = []
zipWithComponents as            []            bd f = (fmap.fmap) (`f` bd) as
zipWithComponents (Word a:as)   (Word b:bs)   bd f = Word (f a b) : zipWithComponents as bs bd f
zipWithComponents as@(Word _:_) (_:bs)        bd f = zipWithComponents as bs bd f
zipWithComponents (a:as)        bs@(Word _:_) bd f = unsafeCastComponent a : zipWithComponents as bs bd f
zipWithComponents (a:as)        (_:bs)        bd f = unsafeCastComponent a : zipWithComponents as bs bd f
