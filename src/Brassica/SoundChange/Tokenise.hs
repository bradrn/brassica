{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Brassica.SoundChange.Tokenise
       ( Component(..)
       , getWords
       , zipWithComponents
       , tokeniseWord
       , tokeniseWords
       , wordParser
       , componentsParser
       , findFirstCategoriesDecl
       , withFirstCategoriesDecl
       , detokeniseWords'
       , detokeniseWords
       ) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.Functor.Identity
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Void (Void)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Brassica.SoundChange.Types

-- | Represents a component of a tokenised input string. The type
-- variable will usually be something like '[Grapheme]', though it
-- depends on the type of words you’re parsing.
data Component a = Word a | Whitespace String | Gloss String
    deriving (Eq, Show, Functor, Foldable, Traversable, Generic, NFData)

-- | Given a tokenised input string, return only the 'Word's within
-- it.
getWords :: [Component a] -> [a]
getWords = mapMaybe $ \case
    Word a -> Just a
    _ -> Nothing

-- | Megaparsec parser for 'Words' — see 'tokeniseWord' documentation
-- for details on the parsing strategy. For most usecases
-- 'tokeniseWord' should suffice; 'wordParser' itself is only really
-- useful in unusual situations (e.g. as part of a larger parser). The
-- first parameter gives a list of characters (aside from whitespace)
-- which should be excluded from words, i.e. the parser will stop if
-- any of them are found.
wordParser :: [Char] -> [Grapheme] -> ParsecT Void String Identity [Grapheme]
wordParser excludes gs = some $ choice (chunk <$> gs) <|> (pure <$> satisfy (not . exclude))
  where
    exclude = (||) <$> isSpace <*> (`elem` excludes)

-- | Megaparsec parser for 'Component's. Similarly to 'wordParser',
-- usually it’s easier to use 'tokeniseWords' instead.
componentsParser
    :: ParsecT Void String Identity a
    -> ParsecT Void String Identity [Component a]
componentsParser p = many $
    (Whitespace <$> takeWhile1P Nothing isSpace) <|>
    (Gloss <$> gloss False) <|>
    (Word <$> p)
  where
    gloss returnBracketed = do
        _ <- char '['
        contents <- many $ gloss True <|> takeWhile1P Nothing (not . (`elem` "[]"))
        _ <- char ']'
        pure $ if returnBracketed
           then '[' : concat contents ++ "]"
           else concat contents

-- | Given a list of 'Grapheme's used, tokenise an input word. Note
-- that this tokeniser is greedy: if one of the given 'Grapheme's is a
-- prefix of another, the tokeniser will prefer the longest if possible.
tokeniseWord :: [Grapheme] -> String -> Either (ParseErrorBundle String Void) [Grapheme]
tokeniseWord (sortBy (compare `on` Down . length) -> gs) = parse (wordParser "[" gs) ""

-- | Given a list of 'Grapheme's used, tokenise an input string into a
-- list of words and other 'Component's. This uses the same
-- tokenisation strategy as 'tokeniseWords'.
tokeniseWords :: [Grapheme] -> String -> Either (ParseErrorBundle String Void) [Component [Grapheme]]
tokeniseWords (sortBy (compare `on` Down . length) -> gs) =
    parse (componentsParser $ wordParser "[" gs) ""

-- | Given a function to convert words to strings, converts a list of
-- 'Component's to strings.
detokeniseWords' :: (a -> String) -> [Component a] -> String
detokeniseWords' f = concatMap $ \case
    Word gs -> f gs
    Whitespace w -> w
    Gloss g -> '[':(g ++ "]")

-- | Specialisation of 'detokeniseWords'' for words which are
-- '[Grapheme]', converting words to strings using 'concat'.
detokeniseWords :: [Component [Grapheme]] -> String
detokeniseWords = detokeniseWords' concat

-- | Zips two tokenised input strings. Compared to normal 'zipWith'
-- this has two special properties:
--
--   * It only zips 'Word's. Any non-'Word's in the first argument
--     will be passed unaltered to the output; any in the second
--     argument will be ignored.
--
--   * The returned list will have the same number of elements as does
--     the first argument. If a 'Word' in the first argument has no
--     corresponding 'Word' in the second, the zipping function is
--     called using the default @b@ value given as the third argument.
--     Such a 'Word' in the second argument will simply be ignored.
--
-- Note the persistent assymetry in the definition: each 'Component'
-- in the first argument will be reflected in the output, but each in
-- the second argument may be ignored.
zipWithComponents :: [Component a] -> [Component b] -> b -> (a -> b -> c) -> [Component c]
zipWithComponents []             _            _  _ = []
zipWithComponents as            []            bd f = (fmap.fmap) (`f` bd) as
zipWithComponents (Word a:as)   (Word b:bs)   bd f = Word (f a b) : zipWithComponents as bs bd f
zipWithComponents as@(Word _:_) (_:bs)        bd f = zipWithComponents as bs bd f
zipWithComponents (a:as)        bs@(Word _:_) bd f = unsafeCastComponent a : zipWithComponents as bs bd f
zipWithComponents (a:as)        (_:bs)        bd f = unsafeCastComponent a : zipWithComponents as bs bd f

unsafeCastComponent :: Component a -> Component b
unsafeCastComponent (Word _) = error "unsafeCastComponent: attempted to cast a word!"
unsafeCastComponent (Whitespace s) = Whitespace s
unsafeCastComponent (Gloss s) = Gloss s

-- | Given a list of sound changes, extract the list of graphemes
-- defined in the first categories declaration.
findFirstCategoriesDecl :: SoundChanges -> [Grapheme]
findFirstCategoriesDecl (CategoriesDeclS (CategoriesDecl gs):_) = gs
findFirstCategoriesDecl (_:ss) = findFirstCategoriesDecl ss
findFirstCategoriesDecl [] = []

-- | CPS'd form of 'findFirstCategoriesDecl'. Nice for doing things
-- like @withFirstCategoriesDecl 'tokeniseWords' changes words@ and so
-- on.
withFirstCategoriesDecl :: ([Grapheme] -> t) -> SoundChanges -> t
withFirstCategoriesDecl tok ss = tok (findFirstCategoriesDecl ss)
