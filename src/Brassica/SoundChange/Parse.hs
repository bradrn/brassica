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
import Data.List (dropWhileEnd)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
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

-- space consumer which does not match newlines or comments
sc :: Parser ()
sc = L.space space1' empty empty
  where
    -- adapted from megaparsec source: like 'space1', but does not
    -- consume newlines (which are important for rule separation)
    space1' = void $ takeWhile1P (Just "white space") ((&&) <$> isSpace <*> (/='\n'))

-- space consumer which matches newlines and comments
scn :: Parser ()
scn = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keyChars :: [Char]
keyChars = "#[](){}>\\→/_^%~*@$;"

nonzero :: Parser Int
nonzero = label "nonzero postive number" $ try $ do
    n <- lexeme L.decimal
    guard $ n>0
    pure n

parseGrapheme :: Parser Grapheme
parseGrapheme = lexeme $
    pure <$> char '#'
    <|> parseGrapheme' True

parseGrapheme' :: Bool -> Parser String
parseGrapheme' wantTilde = lexeme $ do
    star <- optional (char '*')
    rest <- takeWhile1P Nothing (not . ((||) <$> isSpace <*> (`elem` keyChars)))
    nocat <-
        if wantTilde
        then optional (char '~')
        else pure Nothing
    pure .
        maybe id (const ('*':)) star .
        maybe id (const (++"~")) nocat
        $ rest

parseExplicitCategory :: ParseLexeme a => Parser (Lexeme CategorySpec a)
parseExplicitCategory = Category <$> parseExplicitCategory'

parseGreedyCategory :: Parser (Lexeme CategorySpec 'Matched)
parseGreedyCategory = GreedyCategory <$> (char '%' *> parseExplicitCategory')

parseExplicitCategory' :: ParseLexeme a => Parser (CategorySpec a)
parseExplicitCategory' = fmap CategorySpec $
    (:) <$> (symbol "[" *> parseCategoryModification True)
        <*> manyTill (parseCategoryModification False) (symbol "]")

-- This is unused currently, but convenient to keep around just in case
-- parseCategory :: ParseLexeme a => Parser (Lexeme CategorySpec a)
-- parseCategory = Category <$> parseCategory'

parseCategory' :: ParseLexeme a => Parser (CategorySpec a)
parseCategory' = parseExplicitCategory' <|> MustInline <$> parseGrapheme' True

parseCategoryStandalone
    :: Parser (String, CategorySpec 'AnyPart)
parseCategoryStandalone = do
    g <- parseGrapheme' True
    _ <- symbol "="
    mods <- some (parseCategoryModification False)
    return (g, CategorySpec mods)

parseFeature :: Parser FeatureSpec
parseFeature = do
    _ <- symbol "feature"
    featureBaseName <- optional $ try $ parseGrapheme' False <* symbol "="
    featureBaseValues <- CategorySpec <$> some (parseCategoryModification False)
    featureDerived <- some (symbol "/" *> parseCategoryStandalone) <* scn
    pure FeatureSpec { featureBaseName, featureBaseValues, featureDerived }

parseAuto :: Parser String
parseAuto = symbol "auto" *> parseGrapheme' False <* scn

parseCategoryModification
    :: ParseLexeme a
    => Bool
    -> Parser (CategoryModification, Either Grapheme [Lexeme CategorySpec a])
parseCategoryModification forceUnion = (,)
    <$> (if forceUnion
           then Union <$ optional (char '&')
           else parsePrefix)
    <*> ((Right <$> (symbol "{" *> manyTill parseLexeme (symbol "}")))
        <|> (Left <$> parseGrapheme))
  where
    parsePrefix =
        (Intersect <$ char '+')
        <|> (Subtract <$ char '-')
        <|> (Union <$ char '&')  -- necessary for featural categories
        <|> pure Union

parseDirective :: Parser Directive
parseDirective = parseCategoriesDirective <|> parseExtraDirective
  where
    parseExtraDirective = fmap ExtraGraphemes $
        symbol "extra" *> many (parseGrapheme' False) <* scn

    parseCategoriesDirective = do
        overwrite <- isJust <$> optional (symbol "new")
        _ <- symbol "categories"
        noreplace <- isJust <$> optional (symbol "noreplace")
        scn
        cs <- some $
            DefineFeature <$> parseFeature <|>
            DefineAuto <$> parseAuto <|>
            uncurry DefineCategory <$> (try parseCategoryStandalone <* scn)
        _ <- symbol "end" <* scn
        pure $ Categories overwrite noreplace cs

parseOptional :: ParseLexeme a => Parser (Lexeme CategorySpec a)
parseOptional = Optional <$> between (symbol "(") (symbol ")") (some parseLexeme)

parseGreedyOptional :: Parser (Lexeme CategorySpec 'Matched)
parseGreedyOptional = GreedyOptional <$> between (symbol "%(") (symbol ")") (some parseLexeme)

parseGeminate :: Parser (Lexeme CategorySpec a)
parseGeminate = Geminate <$ symbol ">"

parseMetathesis :: Parser (Lexeme CategorySpec 'Replacement)
parseMetathesis = Metathesis <$ symbol "\\"

parseWildcard :: ParseLexeme a => Parser (Lexeme CategorySpec a)
parseWildcard = Wildcard <$> (symbol "^" *> parseLexeme)

parseDiscard :: Parser (Lexeme CategorySpec 'Replacement)
parseDiscard = Discard <$ symbol "~"

parsePost :: Lexeme CategorySpec a -> Parser (Lexeme CategorySpec a)
parsePost l =
    try parseFeatureApp
    <|> try (lexeme $ Kleene l <$ char '*' <* notFollowedBy (parseGrapheme' True))
    <|> pure l
  where
    parseFeatureApp =
        Feature <$ char '$'
        <*> parseGrapheme' False
        <*> optional (char '#' *> parseGrapheme' False)
        <*> fmap (fromMaybe [])
            ( optional $ between (symbol "(") (symbol ")") $
              many $ lexeme $ parseGrapheme' False `sepBy1` char '~'
            )
        <*> pure l

parseMultiple :: Parser (Lexeme CategorySpec 'Replacement)
parseMultiple = Multiple <$> (symbol "@?" *> parseCategory')

parseBackreference :: forall a. ParseLexeme a => Parser (Lexeme CategorySpec a)
parseBackreference = Backreference <$> (symbol "@" *> ref) <*> parseCategory'
  where
    ref =
        Left <$> (char '#' *> parseGrapheme' False)
        <|> Right <$> nonzero

instance ParseLexeme 'Matched where
    parseLexeme = asum
        [ parseExplicitCategory
        , parseOptional
        , parseGreedyOptional
        , parseGreedyCategory
        , parseGeminate
        , parseWildcard
        , parseBackreference
        , Grapheme <$> parseGrapheme
        ] >>= parsePost

instance ParseLexeme 'Replacement where
    parseLexeme = asum
        [ parseExplicitCategory
        , parseOptional
        , parseMetathesis
        , parseDiscard
        , parseGeminate
        , parseMultiple
        , parseWildcard
        , parseBackreference
        , Grapheme <$> parseGrapheme
        ] >>= parsePost

instance ParseLexeme 'AnyPart where
    parseLexeme = asum
        [ parseExplicitCategory
        , parseOptional
        , parseWildcard
        , Grapheme <$> parseGrapheme
        ] >>= parsePost

parseLexemes :: ParseLexeme a => Parser [Lexeme CategorySpec a]
parseLexemes = many parseLexeme

parseFlags :: Parser Flags
parseFlags = runPermutation $ Flags
    <$> toPermutation (isNothing <$> optional (symbol "-x"))
    <*> toPermutationWithDefault LTR ((LTR <$ symbol "-ltr") <|> (RTL <$ symbol "-rtl"))
    <*> toPermutation (isJust <$> optional (symbol "-1"))
    <*> toPermutationWithDefault ApplyAlways
        ((PerApplication <$ symbol "-??") <|> (PerWord <$ symbol "-?"))
    <*> toPermutation (isJust <$> optional (symbol "-no"))

ruleParser :: Parser (Rule CategorySpec)
ruleParser = do
    -- This is an inlined version of 'match' from @megaparsec@;
    -- 'match' itself would be tricky to use here, since it would need
    -- to wrap multiple parsers rather than just one
    o <- getOffset
    s <- getInput

    flags <- parseFlags
    target <- manyTill parseLexeme $ lexeme $ choice
        [ string "/"
        , string "→"
        , string "->"
        ]
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

    o' <- getOffset

    _ <- optional scn   -- consume newline after rule if present

    let plaintext = dropWhile isSpace $ dropWhileEnd isSpace $
            (fst . fromJust) (takeN_ (o' - o) s)
    return Rule{environment=envs, ..}

filterParser :: Parser (Filter CategorySpec)
filterParser = fmap (uncurry Filter) $ match $ symbol "filter" *> parseLexemes <* optional scn

-- Space handline is a little complex here: we want to make sure that
-- 'report' is always on its own line, but can have as much or as
-- little space after it as needed
reportParser :: Parser ()
reportParser = symbol "report" *> sc *> ((newline *> void (optional scn)) <|> eof)

-- | Parse a 'String' in Brassica sound change syntax into a
-- 'Rule'. Returns 'Left' if the input string is malformed.
--
-- For details on the syntax, refer to <https://github.com/bradrn/brassica/blob/v0.3.0/Documentation.md#basic-rule-syntax>.
parseRule :: String -> Either (ParseErrorBundle String Void) (Rule CategorySpec)
parseRule = runParser (scn *> ruleParser <* eof) ""

-- | Parse a list of 'SoundChanges'.
parseSoundChanges :: String -> Either (ParseErrorBundle String Void) (SoundChanges CategorySpec Directive)
parseSoundChanges = runParser (scn *> parser <* eof) ""
  where
    parser = many $
        DirectiveS <$> parseDirective
        <|> FilterS <$> filterParser
        <|> ReportS <$ reportParser
        <|> RuleS <$> ruleParser
