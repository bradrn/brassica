{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module SoundChange.Parse
    ( ParseLexeme
    , parseRule
    , parseRules
    , parseCategorySpec
    , parseCategoriesSpec
    , Component(..)
    , getWords
    , unsafeCastComponent
    , zipWithComponents
    , tokeniseWords
    , detokeniseWords
    , detokeniseWords'
    , parseSupra
    , Config(..)
      -- * Re-exports
    , errorBundlePretty
    ) where

import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Foldable (asum)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (isNothing, isJust, fromJust, mapMaybe)
import Data.Ord (Down(..))
import Data.Void (Void)

import Control.Applicative.Permutations
import Control.Monad.Reader
import qualified Data.List.Split as S
import qualified Data.Map.Strict as M

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import SoundChange.Types
import qualified SoundChange.Category as C

data Config = Config
    { categories :: C.Categories Grapheme
    }
type Parser = ParsecT Void String (Reader Config)

class ParseLexeme (a :: LexemeType) where
    parseLexeme :: Parser (Lexeme a)
    parseCategoryElement :: Parser (CategoryElement a)

-- space consumer which does not match newlines
sc :: Parser ()
sc = L.space space1' (L.skipLineComment "*") empty
  where
    -- adapted from megaparsec source: like 'space1', but does not
    -- consume newlines (which are important for rule separation)
    space1' = void $ takeWhile1P (Just "white space") ((&&) <$> isSpace <*> (/='\n'))

-- space consumer which matches newlines
scn :: Parser ()
scn = L.space space1 (L.skipLineComment "*") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keyChars :: [Char]
keyChars = "#[](){}>\\/_^%"

parseGrapheme :: Parser Grapheme
parseGrapheme = lexeme $ takeWhile1P Nothing (not . ((||) <$> isSpace <*> (`elem` keyChars)))

parseGrapheme' :: Parser Grapheme
parseGrapheme' = lexeme $ takeWhile1P Nothing (not . ((||) <$> isSpace <*> (=='=')))

data CategoryModification a
    = Union     (CategoryElement a)
    | Intersect (CategoryElement a)
    | Subtract  (CategoryElement a)

parseGraphemeOrCategory :: ParseLexeme a => Parser (Lexeme a)
parseGraphemeOrCategory = do
    g <- parseGrapheme
    cats <- asks categories
    return $ case C.lookup g cats of
        Nothing -> Grapheme g
        Just c  -> Category $ C.bake $ GraphemeEl <$> c

parseCategory :: ParseLexeme a => Parser (Lexeme a)
parseCategory = do
    mods <- symbol "[" *> someTill parseCategoryModification (symbol "]")
    cats <- asks categories
    return $ Category $ C.bake $
        C.expand (C.mapCategories GraphemeEl cats) (toCategory mods)

parseCategoryStandalone :: Parser (Grapheme, C.Category 'C.Expanded Grapheme)
parseCategoryStandalone = do
    g <- parseGrapheme'
    _ <- symbol "="
    mods <- some (parseCategoryModification @'Target)
    cats <- asks categories
    return (g, C.expand cats $ toGrapheme <$> toCategory mods)
  where
    -- Use Target here because it only allows graphemes, not boundaries
    toGrapheme :: CategoryElement 'Target -> Grapheme
    toGrapheme (GraphemeEl g) = g

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

parseWithinSyllable :: (ParseLexeme a, OneOf a 'Target 'Env) => Parser (Lexeme a)
parseWithinSyllable = WithinSyllable <$> (symbol "^^" *> parseLexeme)

parseBoundary :: Parser ()
parseBoundary = () <$ symbol "#"

parseSyllable :: Parser (Lexeme a)
parseSyllable = Syllable <$ symbol "%"

parseSupra :: Parser (Lexeme a)
parseSupra = Supra <$> between (symbol "{") (symbol "}") (some $ absence <|> try kvpair)
  where
    absence :: Parser (String, Maybe String)
    absence = (,Nothing) <$> (char '!' *> parseGrapheme)

    kvpair :: Parser (String, Maybe String)
    kvpair = (.Just) . (,) <$> parseGrapheme' <* symbol "=" <*> parseGrapheme

instance ParseLexeme 'Target where
    parseLexeme = asum
        [ parseCategory
        , parseOptional
        , parseGeminate
        , parseWithinSyllable
        , parseWildcard
        , parseSyllable
        , parseSupra
        , parseGraphemeOrCategory
        ]
    parseCategoryElement = GraphemeEl <$> parseGrapheme

instance ParseLexeme 'Replacement where
    parseLexeme = asum
        [ parseCategory
        , parseOptional
        , parseMetathesis
        , parseGeminate
        , parseSyllable
        , parseSupra
        , parseGraphemeOrCategory
        ]
    parseCategoryElement = GraphemeEl <$> parseGrapheme

instance ParseLexeme 'Env where
    parseLexeme = asum
        [ parseCategory
        , Boundary <$ parseBoundary
        , parseOptional
        , parseGeminate
        , parseWithinSyllable
        , parseWildcard
        , parseSyllable
        , parseSupra
        , parseGraphemeOrCategory
        ]
    parseCategoryElement = asum
        [ BoundaryEl <$  parseBoundary
        , GraphemeEl <$> parseGrapheme
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
    _ <- symbol "/"
    replacement <- parseLexemes
    _ <- symbol "/"
    env1 <- parseLexemes
    _ <- symbol "_"
    env2 <- parseLexemes
    exception <- optional $ (,) <$> (symbol "/" *> parseLexemes) <* symbol "_" <*> parseLexemes
    _ <- optional scn   -- consume newline after rule if present

    o' <- getOffset
    let plaintext = (fst . fromJust) (takeN_ (o' - o) s)
    return Rule{environment=(env1,env2), ..}

-- | Parse a 'String' to get a 'Rule'. Returns 'Nothing' if the input
-- string is malformed.
parseRule
    :: C.Categories Grapheme    -- ^ A set of categories which have been pre-defined
    -> String                   -- ^ The string to parse
    -> Either (ParseErrorBundle String Void) Rule
parseRule cats s = flip runReader (Config cats) $ runParserT (scn *> ruleParser <* eof) "" s

-- | Parse a list of rules.
parseRules :: C.Categories Grapheme -> String -> Either (ParseErrorBundle String Void) [Rule]
parseRules cats s = flip runReader (Config cats) $ runParserT (scn *> many ruleParser <* eof) "" s

-- | Parse a category specification, yielding the name of that
-- category as well as a list of elements present in that
-- category. Returns 'Nothing' if the specification cannot be parsed.
parseCategorySpec
    :: C.Categories Grapheme   -- ^ A set of categories which have been pre-defined
    -> String                  -- ^ The string to parse
    -> Maybe (Grapheme, C.Category 'C.Expanded Grapheme)
parseCategorySpec cats s =
    case flip runReader (Config cats) $ runParserT (parseCategoryStandalone <* eof) "" s of
        Right es -> Just es
        Left  _  -> Nothing

-- | Parse a list of category specifications, accumulating them into
-- a list of 'Categories'.
parseCategoriesSpec :: [String] -> C.Categories Grapheme
parseCategoriesSpec = flip foldl M.empty $ \cs s -> case parseCategorySpec cs s of
    Nothing    -> cs
    Just (k,c) -> M.insert k c cs

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

tokeniseWords :: [Grapheme] -> String -> [Component [Grapheme]]
tokeniseWords (sortBy (compare `on` Down . length) -> gs) =
    fromJust . parseMaybe @Void (many $
        (Whitespace <$> takeWhile1P Nothing isSpace) <|>
        (Gloss <$> (char '[' *> takeWhileP Nothing (/=']') <* char ']')) <|>
        (Word <$> parseWord))
  where
    parseWord = some $ choice (chunk <$> gs) <|> (pure <$> satisfy (not . isSpace))

detokeniseWords :: [Component [WordPart]] -> String
detokeniseWords = detokeniseWords' $ concatMap $ fromRight ""

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
