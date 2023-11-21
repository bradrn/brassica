{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Brassica.SoundChange.Parse
    ( parseRule
    , parseRuleWithCategories
    , parseSoundChanges
      -- ** Re-export
    , errorBundlePretty
    ) where

import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.List (transpose)
import Data.Maybe (isNothing, isJust, fromJust)
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
keyChars = "#[](){}>\\→/_^%~*@"

nonzero :: Parser Int
nonzero = label "nonzero postive number" $ try $ do
    n <- lexeme L.decimal
    guard $ n>0
    pure n

parseGrapheme :: Parser (Grapheme, Bool)
parseGrapheme = lexeme $ parseBoundary <|> parseMulti
  where
    parseBoundary = (GBoundary,False) <$ char '#'

    parseMulti = (,)
        <$> fmap GMulti
            (withStar $ takeWhile1P Nothing (not . ((||) <$> isSpace <*> (`elem` keyChars))))
        <*> (isJust <$> optional (char '~'))

    withStar :: Parser String -> Parser String
    withStar p = optional (char '*') >>= \case
        Just _ -> ('*':) <$> p
        Nothing -> p

parseGrapheme' :: Parser Grapheme
parseGrapheme' = lexeme $ GMulti <$> takeWhile1P Nothing (not . ((||) <$> isSpace <*> (=='=')))

data CategoryModification
    = Union     Grapheme
    | Intersect Grapheme
    | Subtract  Grapheme

parseGraphemeOrCategory :: ParseLexeme a => Parser (Lexeme a)
parseGraphemeOrCategory = do
    (g, isntCat) <- parseGrapheme
    if isntCat
        then return $ Grapheme g
        else do
            cats <- gets categories
            return $ case C.lookup g cats of
                Nothing -> Grapheme g
                Just c  -> Category $ C.bake c

parseCategory :: ParseLexeme a => Parser (Lexeme a)
parseCategory = Category <$> parseCategory'

parseCategory' :: Parser [Grapheme]
parseCategory' = do
    mods <- symbol "[" *> someTill parseCategoryModification (symbol "]")
    cats <- gets categories
    return $ C.bake $
        C.expand cats (toCategory mods)

parseCategoryStandalone :: Parser (Grapheme, C.Category 'C.Expanded Grapheme)
parseCategoryStandalone = do
    g <- parseGrapheme'
    _ <- symbol "="
    -- Use Target here because it only allows graphemes, not boundaries
    mods <- some parseCategoryModification
    cats <- gets categories
    return (g, C.expand cats $ toCategory mods)

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
        modsPlain <- some parseCategoryModification
        cats <- gets categories
        let plainCat = C.expand cats $ toCategory modsPlain
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

parseCategoryModification :: Parser CategoryModification
parseCategoryModification = parsePrefix <*> (fst <$> parseGrapheme)
  where
    parsePrefix =
        (Intersect <$ char '+')
        <|> (Subtract <$ char '-')
        <|> pure Union

toCategory :: [CategoryModification] -> C.Category 'C.Unexpanded Grapheme
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

parseDiscard :: Parser (Lexeme 'Replacement)
parseDiscard = Discard <$ symbol "~"

parseKleene :: OneOf a 'Target 'Env => Lexeme a -> Parser (Lexeme a)
parseKleene l =
    try (lexeme $ Kleene l <$ char '*' <* notFollowedBy parseGrapheme')
    <|> pure l

parseMultiple :: Parser (Lexeme 'Replacement)
parseMultiple = Multiple <$> (symbol "@?" *> parseCategory')

parseBackreference
    :: forall a.
       (OneOf a 'Target 'Replacement, ParseLexeme a)
    => Parser (Lexeme a)
parseBackreference =
    Backreference
    <$> (symbol "@" *> nonzero)
    <*> (parseCategory' <|> parseGraphemeCategory)
  where
    parseGraphemeCategory :: Parser [Grapheme]
    parseGraphemeCategory = label "category" $ try $
        (parseGraphemeOrCategory @a) >>= \case
            Category gs -> pure gs
            _ -> empty

instance ParseLexeme 'Target where
    parseLexeme = asum
        [ parseCategory
        , parseOptional
        , parseGeminate
        , parseWildcard
        , parseBackreference
        , parseGraphemeOrCategory
        ] >>= parseKleene

instance ParseLexeme 'Replacement where
    parseLexeme = asum
        [ parseCategory
        , parseOptional
        , parseMetathesis
        , parseDiscard
        , parseGeminate
        , parseMultiple
        , parseBackreference
        , parseGraphemeOrCategory
        ]

instance ParseLexeme 'Env where
    parseLexeme = asum
        [ parseCategory
        , parseOptional
        , parseGeminate
        , parseWildcard
        , parseGraphemeOrCategory
        ] >>= parseKleene

parseLexemes :: ParseLexeme a => Parser [Lexeme a]
parseLexemes = many parseLexeme

parseFlags :: Parser Flags
parseFlags = runPermutation $ Flags
    <$> toPermutation (isNothing <$> optional (symbol "-x"))
    <*> toPermutationWithDefault LTR ((LTR <$ symbol "-ltr") <|> (RTL <$ symbol "-rtl"))
    <*> toPermutation (isJust <$> optional (symbol "-1"))
    <*> toPermutation (isJust <$> optional (symbol "-?"))

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

    envs' <- many $ do
        notFollowedBy $ symbol "//"  -- for exceptions
        _ <- symbol "/"
        env1 <- parseLexemes
        _ <- symbol "_"
        env2 <- parseLexemes
        return (env1, env2)
    let envs = if null envs' then [([], [])] else envs'

    exception <- optional $ (,) <$> (symbol "//" *> parseLexemes) <* symbol "_" <*> parseLexemes

    _ <- optional scn   -- consume newline after rule if present

    o' <- getOffset
    let plaintext = takeWhile notNewline $ (fst . fromJust) (takeN_ (o' - o) s)
    return Rule{environment=envs, ..}
  where
    notNewline c = (c /= '\n') && (c /= '\r')

-- | Parse a 'String' in Brassica sound change syntax into a
-- 'Rule'. Returns 'Left' if the input string is malformed.
--
-- For details on the syntax, refer to <https://github.com/bradrn/brassica/blob/v0.1.1/Documentation.md#basic-rule-syntax>.
parseRule :: String -> Either (ParseErrorBundle String Void) Rule
parseRule = parseRuleWithCategories M.empty

-- | Same as 'parseRule', but also allows passing in some predefined
-- categories to substitute.
parseRuleWithCategories :: C.Categories Grapheme -> String -> Either (ParseErrorBundle String Void) Rule
parseRuleWithCategories cs s = flip evalState (Config cs) $ runParserT (scn *> ruleParser <* eof) "" s

-- | Parse a list of 'SoundChanges'.
parseSoundChanges :: String -> Either (ParseErrorBundle String Void) SoundChanges
parseSoundChanges s = flip evalState (Config M.empty) $ runParserT (scn *> parser <* eof) "" s
  where
    parser = many $
        CategoriesDeclS <$> categoriesDeclParse
        <|> RuleS <$> ruleParser
