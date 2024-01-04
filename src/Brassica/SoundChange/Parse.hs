{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Brassica.SoundChange.Parse
    ( parseRule
    , parseSoundChanges
      -- ** Re-export
    , errorBundlePretty
    ) where

import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.Maybe (isNothing, isJust, fromJust)
import Data.Void (Void)

import Control.Applicative.Permutations
import Control.Monad (void, guard)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Brassica.SoundChange.Types

type Parser = Parsec Void String

class ParseLexeme (a :: LexemeType) where
    parseLexeme :: Parser (Lexeme CategorySpec a)

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

parseGrapheme :: Parser Grapheme
parseGrapheme = lexeme $
    GBoundary <$ char '#'
    <|> GMulti <$> parseGrapheme'

parseGrapheme' :: Parser String
parseGrapheme' = lexeme $ do
    star <- optional (char '*')
    rest <- takeWhile1P Nothing (not . ((||) <$> isSpace <*> (`elem` keyChars)))
    nocat <- optional (char '~')
    pure .
        maybe id (const ('*':)) star .
        maybe id (const (++"~")) nocat
        $ rest

parseExplicitCategory :: ParseLexeme a => Parser (Lexeme CategorySpec a)
parseExplicitCategory = Category <$> parseExplicitCategory'

parseExplicitCategory' :: ParseLexeme a => Parser (CategorySpec a)
parseExplicitCategory' =
    CategorySpec <$> (symbol "[" *> someTill parseCategoryModification (symbol "]"))

-- This is unused currently, but convenient to keep around just in case
-- parseCategory :: ParseLexeme a => Parser (Lexeme CategorySpec a)
-- parseCategory = Category <$> parseCategory'

parseCategory' :: ParseLexeme a => Parser (CategorySpec a)
parseCategory' = parseExplicitCategory' <|> MustInline <$> parseGrapheme'

parseCategoryStandalone
    :: Parser (String, CategorySpec 'AnyPart)
parseCategoryStandalone = do
    g <- parseGrapheme'
    _ <- symbol "="
    mods <- some parseCategoryModification
    return (g, CategorySpec mods)

parseFeature :: Parser FeatureSpec
parseFeature = do
    _ <- symbol "feature"
    featureBaseName <- optional $ try $ parseGrapheme' <* symbol "="
    featureBaseValues <- CategorySpec <$> some parseCategoryModification
    featureDerived <- some (symbol "/" *> parseCategoryStandalone) <* scn
    pure FeatureSpec { featureBaseName, featureBaseValues, featureDerived }

parseCategoryModification
    :: ParseLexeme a
    => Parser (CategoryModification, Either Grapheme [Lexeme CategorySpec a])
parseCategoryModification = (,)
    <$> parsePrefix
    <*> ( (Right <$> (symbol "{" *> manyTill parseLexeme (symbol "}")))
        <|> (Left <$> parseGrapheme))
  where
    parsePrefix =
        (Intersect <$ char '+')
        <|> (Subtract <$ char '-')
        <|> pure Union

parseDirective :: Parser Directive
parseDirective = do
    overwrite <- isJust <$> optional (symbol "new")
    _ <- symbol "categories" <* scn
    cs <- some $
        DefineFeature <$> parseFeature <|>
        uncurry DefineCategory <$> (try parseCategoryStandalone <* scn)
    _ <- symbol "end" <* scn
    pure $ Categories overwrite cs

parseOptional :: ParseLexeme a => Parser (Lexeme CategorySpec a)
parseOptional = Optional <$> between (symbol "(") (symbol ")") (some parseLexeme)

parseGeminate :: Parser (Lexeme CategorySpec a)
parseGeminate = Geminate <$ symbol ">"

parseMetathesis :: Parser (Lexeme CategorySpec 'Replacement)
parseMetathesis = Metathesis <$ symbol "\\"

parseWildcard :: (ParseLexeme a, OneOf a 'Target 'Env) => Parser (Lexeme CategorySpec a)
parseWildcard = Wildcard <$> (symbol "^" *> parseLexeme)

parseDiscard :: Parser (Lexeme CategorySpec 'Replacement)
parseDiscard = Discard <$ symbol "~"

parseKleene :: OneOf a 'Target 'Env => Lexeme CategorySpec a -> Parser (Lexeme CategorySpec a)
parseKleene l =
    try (lexeme $ Kleene l <$ char '*' <* notFollowedBy parseGrapheme')
    <|> pure l

parseMultiple :: Parser (Lexeme CategorySpec 'Replacement)
parseMultiple = Multiple <$> (symbol "@?" *> parseCategory')

parseBackreference :: forall a. ParseLexeme a => Parser (Lexeme CategorySpec a)
parseBackreference = Backreference <$> (symbol "@" *> nonzero) <*> parseCategory'

instance ParseLexeme 'Target where
    parseLexeme = asum
        [ parseExplicitCategory
        , parseOptional
        , parseGeminate
        , parseWildcard
        , parseBackreference
        , Grapheme <$> parseGrapheme
        ] >>= parseKleene

instance ParseLexeme 'Replacement where
    parseLexeme = asum
        [ parseExplicitCategory
        , parseOptional
        , parseMetathesis
        , parseDiscard
        , parseGeminate
        , parseMultiple
        , parseBackreference
        , Grapheme <$> parseGrapheme
        ]

instance ParseLexeme 'Env where
    parseLexeme = asum
        [ parseExplicitCategory
        , parseOptional
        , parseGeminate
        , parseWildcard
        , parseBackreference
        , Grapheme <$> parseGrapheme
        ] >>= parseKleene

instance ParseLexeme 'AnyPart where
    parseLexeme = asum
        [ parseExplicitCategory
        , parseOptional
        , Grapheme <$> parseGrapheme
        ]

parseLexemes :: ParseLexeme a => Parser [Lexeme CategorySpec a]
parseLexemes = many parseLexeme

parseFlags :: Parser Flags
parseFlags = runPermutation $ Flags
    <$> toPermutation (isNothing <$> optional (symbol "-x"))
    <*> toPermutationWithDefault LTR ((LTR <$ symbol "-ltr") <|> (RTL <$ symbol "-rtl"))
    <*> toPermutation (isJust <$> optional (symbol "-1"))
    <*> toPermutation (isJust <$> optional (symbol "-?"))

ruleParser :: Parser (Rule CategorySpec)
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
-- For details on the syntax, refer to <https://github.com/bradrn/brassica/blob/v0.2.0/Documentation.md#basic-rule-syntax>.
parseRule :: String -> Either (ParseErrorBundle String Void) (Rule CategorySpec)
parseRule = runParser (scn *> ruleParser <* eof) ""

-- | Parse a list of 'SoundChanges'.
parseSoundChanges :: String -> Either (ParseErrorBundle String Void) (SoundChanges CategorySpec Directive)
parseSoundChanges = runParser (scn *> parser <* eof) ""
  where
    parser = many $
        DirectiveS <$> parseDirective
        <|> RuleS <$> ruleParser
