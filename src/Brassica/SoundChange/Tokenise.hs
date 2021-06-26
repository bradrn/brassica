{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Brassica.SoundChange.Tokenise
       ( Component(..)
       , getWords
       , unsafeCastComponent
       , zipWithComponents
       , tokeniseWords
       , detokeniseWords
       , detokeniseWords'
       ) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Void (Void)

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Brassica.SoundChange.Types

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

