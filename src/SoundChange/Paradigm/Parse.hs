module SoundChange.Paradigm.Parse
       ( parseParadigm
         -- * Re-exports
       , errorBundlePretty
       ) where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import SoundChange.Paradigm

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1' (L.skipLineComment "*") empty
  where
    -- adapted from megaparsec source: like 'space1', but does not
    -- consume newlines (which are important for rule separation)
    space1' = void $ takeWhile1P (Just "white space") ((&&) <$> isSpace <*> (/='\n'))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

name :: Parser String
name = lexeme $ some alphaNumChar

slot :: Parser Int
slot = lexeme $ L.signed (pure ()) L.decimal

insertion :: Parser Insertion
insertion =
    Null <$ symbol "0"
    <|> Adfix <$> slot <* char '.' <*> lexeme (takeWhile1P (Just "letter") $ (&&) <$> (not.isSpace) <*> (/=')'))

affix :: Parser Affix
affix = fmap pure insertion <|> between (symbol "(") (symbol ")") (some insertion)

grammeme :: Parser Grammeme
grammeme = Concrete <$> affix <|> Abstract <$> name

feature :: Parser Feature
feature = Feature <$> optional (try $ name <* symbol "=") <*> some grammeme <* optional eol

mapping :: Parser ([String], Affix)
mapping = (,) <$> manyTill name (symbol ">") <*> affix <* optional eol

paradigm :: Parser Paradigm
paradigm = Paradigm <$> some feature <*> many mapping

parseParadigm :: String -> Either (ParseErrorBundle String Void) Paradigm
parseParadigm = runParser paradigm ""
