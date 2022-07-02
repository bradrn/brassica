{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Brassica.SoundChange.Tokenise
       ( Component(..)
       , getWords
       , zipWithComponents
       , tokeniseWords
       , detokeniseWords'
       , detokeniseWords
       ) where

import Data.Char (isSpace)
import Data.Function (on)
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
    deriving (Eq, Show, Functor, Generic, NFData)

-- | Given a tokenised input string, return only the 'Word's within
-- it.
getWords :: [Component a] -> [a]
getWords = mapMaybe $ \case
    Word a -> Just a
    _ -> Nothing

-- | Given a list of 'Grapheme's used, tokenise an input string into a
-- list of 'Component's. Note that this tokeniser is greedy: if one of
-- the given 'Grapheme's is a prefix of another, the tokeniser will
-- prefer the longest if possible.
tokeniseWords :: [Grapheme] -> String -> Either (ParseErrorBundle String Void) [Component [Grapheme]]
tokeniseWords (sortBy (compare `on` Down . length) -> gs) =
    parse @Void
        (many $
            (Whitespace <$> takeWhile1P Nothing isSpace) <|>
            (Gloss <$> (char '[' *> takeWhileP Nothing (/=']') <* char ']')) <|>
            (Word <$> parseWord))
        ""
  where
    parseWord = some $ choice (chunk <$> gs) <|> (pure <$> satisfy (not . isSpaceOrGloss))
    isSpaceOrGloss = (||) <$> isSpace <*> (=='[')

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
