module Brassica.Paradigm.Parse (parseParadigm) where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Brassica.Paradigm.Types
import Data.Maybe (fromMaybe)

type Parser = Parsec Void String

-- adapted from megaparsec source: like 'space1', but does not
-- consume newlines (which are important for rule separation)
space1' :: Parser ()
space1' = void $ takeWhile1P (Just "whitespace") ((&&) <$> isSpace <*> (/='\n'))

sc :: Parser ()
sc = L.space space1' (L.skipLineComment "*") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

name :: Parser String
name = lexeme $ some alphaNumChar

slot :: Parser (String -> Process)
slot = do
    n <- L.signed (pure ()) L.decimal
    pure $ if n > 0 then Suffix n else Prefix (-n)

morphValue :: Parser String
morphValue = lexeme (takeWhile1P (Just "letter") $ (&&) <$> (not.isSpace) <*> (/=')'))

process :: Parser Process
process = slot <* char '.' <*> morphValue

oneOrMany :: Parser p -> Parser [p]
oneOrMany p = between (symbol "(") (symbol ")") (many p) <|> fmap pure p

affix :: Parser Affix
affix = oneOrMany process

grammeme :: Parser Grammeme
grammeme =
    Concrete <$> affix
    <|> Abstract . AbstractGrammeme <$> name

condition :: Parser Condition
condition = do
    _ <- symbol "when"
    between (symbol "(") (symbol ")") $
        try (Is . FeatureName <$> name <* symbol "is" <*> grammeme)
        <|> Not . FeatureName <$> name <* symbol "not" <*> grammeme

feature :: Parser Feature
feature = do
    c <- fromMaybe Always <$> optional condition
    globalSlot <- optional $ try $ slot <* space1'
    case globalSlot of
        Nothing -> do
            n <- optional $ try $ name <* symbol "="
            gs <- some grammeme
            _ <- optional eol
            return $ Feature c (FeatureName <$> n) gs
        Just globalSlot' -> do
            n <- optional $ try $ name <* symbol "="
            gs <- some $ oneOrMany $ process <|> (globalSlot' <$> morphValue)
            _ <- optional eol
            return $ Feature c (FeatureName <$> n) (Concrete <$> gs)

mapping :: Parser ([AbstractGrammeme], Affix)
mapping = (,) <$> manyTill (AbstractGrammeme <$> name) (symbol ">") <*> affix <* optional eol

statement :: Parser Statement
statement = sc *>
    (uncurry NewMapping <$> try mapping
    <|> NewFeature <$> feature)

-- | Parse a 'String' in Brassica paradigm syntax into a 'Paradigm'.
-- Returns 'Left' if the input string is malformed.
--
-- For details on the syntax, refer to <https://github.com/bradrn/brassica/blob/v0.2.0/Documentation.md#paradigm-builder>.
parseParadigm :: String -> Either (ParseErrorBundle String Void) Paradigm
parseParadigm = runParser (many statement) ""
