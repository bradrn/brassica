{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
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
    , tokeniseWords
    , detokeniseWords
    , detokeniseWords'
    ) where

import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (isJust, fromJust, mapMaybe)
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

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "*") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

keyChars :: [Char]
keyChars = "#[]()>\\/_^"

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

parseOptional :: (ParseLexeme a, OneOf a 'Target 'Env) => Parser (Lexeme a)
parseOptional = Optional <$> between (symbol "(") (symbol ")") (some parseLexeme)

parseGeminate :: Parser (Lexeme a)
parseGeminate = Geminate <$ symbol ">"

parseMetathesis :: Parser (Lexeme 'Replacement)
parseMetathesis = Metathesis <$ symbol "\\"

parseWildcard :: (ParseLexeme a, OneOf a 'Target 'Env) => Parser (Lexeme a)
parseWildcard = Wildcard <$> (symbol "^" *> parseLexeme)

parseBoundary :: Parser ()
parseBoundary = () <$ symbol "#"

instance ParseLexeme 'Target where
    parseLexeme = asum
        [ parseCategory
        , parseOptional
        , parseGeminate
        , parseWildcard
        , parseGraphemeOrCategory
        ]
    parseCategoryElement = GraphemeEl <$> parseGrapheme

instance ParseLexeme 'Replacement where
    parseLexeme = asum
        [ parseCategory
        , parseMetathesis
        , parseGeminate
        , parseGraphemeOrCategory
        ]
    parseCategoryElement = GraphemeEl <$> parseGrapheme

instance ParseLexeme 'Env where
    parseLexeme = asum
        [ parseCategory
        , Boundary <$ parseBoundary
        , parseOptional
        , parseGeminate
        , parseWildcard
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
    <$> toPermutation (isJust <$> optional (symbol "-x"))
    <*> toPermutationWithDefault LTR ((LTR <$ symbol "-ltr") <|> (RTL <$ symbol "-rtl"))

-- | Parse a 'String' to get a 'Rule'. Returns 'Nothing' if the input
-- string is malformed.
parseRule
    :: C.Categories Grapheme    -- ^ A set of categories which have been pre-defined
    -> String                   -- ^ The string to parse
    -> Maybe Rule
parseRule cats s = case flip runReader (Config cats) $ runParserT (sc *> go <* eof) "" s of
   Right ls -> Just ls
   Left  _  -> Nothing
 where
   go :: Parser Rule
   go = do
       flags <- parseFlags
       target <- parseLexemes
       _ <- symbol "/"
       replacement <- parseLexemes
       _ <- symbol "/"
       env1 <- parseLexemes
       _ <- symbol "_"
       env2 <- parseLexemes
       exception <- optional $ (,) <$> (symbol "/" *> parseLexemes) <* symbol "_" <*> parseLexemes
       return Rule{environment=(env1,env2), ..}

-- | Parse a list of rules. Defined as 'mapMaybe' of 'parseRule'.
parseRules :: C.Categories Grapheme -> [String] -> [Rule]
parseRules cats = mapMaybe (parseRule cats)

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

detokeniseWords :: [Component [Grapheme]] -> String
detokeniseWords = detokeniseWords' concat

detokeniseWords' :: (a -> String) -> [Component a] -> String
detokeniseWords' f = concatMap $ \case
    Word gs -> f gs
    Whitespace w -> w
    Gloss g -> '[':(g ++ "]")
