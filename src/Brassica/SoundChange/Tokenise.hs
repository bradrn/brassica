{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Brassica.SoundChange.Tokenise
       ( 
       -- * Components
         Component(..)
       , getWords
       , splitMultipleResults
       -- * High-level interface
       , tokeniseWord
       , tokeniseWords
       , detokeniseWords'
       , detokeniseWords
       , concatWithBoundary
       , findFirstCategoriesDecl
       , withFirstCategoriesDecl
       -- * Lower-level functions
       , wordParser
       , componentsParser
       , sortByDescendingLength
       ) where

import Data.Char (isSpace)
import Data.Function (on)
import Data.Functor.Identity
import Data.List (intersperse, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..))
import Data.Void (Void)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Brassica.SoundChange.Types

-- | Represents a component of a tokenised input string. v'Word's in
-- the input are represented as the type parameter @a@ — which for
-- this reason will usually, though not always, be 'PWord'.
data Component a = Word a | Separator String | Gloss String
    deriving (Eq, Show, Functor, Foldable, Traversable, Generic, NFData)

-- | Given a tokenised input string, return only the v'Word's within
-- it.
getWords :: [Component a] -> [a]
getWords = mapMaybe $ \case
    Word a -> Just a
    _ -> Nothing

-- | Given a 'Component' containing multiple values in a v'Word',
-- split it apart into a list of 'Component's in which the given
-- 'String' is used as a 'Separator' between multiple results.
--
-- For instance:
--
-- >>> splitMultipleResults " " (Word ["abc", "def", "ghi"])
-- [Word "abc", Separator " ", Word "def", Separator " ", Word "ghi"]
--
-- >>> splitMultipleResults " " (Word ["abc"])
-- [Word "abc"]
splitMultipleResults :: String -> Component [a] -> [Component a]
splitMultipleResults wh (Word as) = intersperse (Separator wh) $ Word <$> as
splitMultipleResults _ (Separator w) = [Separator w]
splitMultipleResults _ (Gloss g) = [Gloss g]
    
-- | Megaparsec parser for 'PWord's — see 'tokeniseWord' documentation
-- for details on the parsing strategy and the meaning of the second
-- parameter. For most usecases 'tokeniseWord' should suffice;
-- 'wordParser' itself is only really useful in unusual situations
-- (e.g. as part of a larger parser). The first parameter gives a list
-- of characters (aside from whitespace) which should be excluded from
-- words, i.e. the parser will stop if any of them are found. The second
-- gives a list of multigraphs which might be expected.
--
-- Note: the second parameter __must__ be 'sortByDescendingLength'-ed;
-- otherwise multigraphs will not be parsed correctly.
wordParser :: [Char] -> [String] -> ParsecT Void String Identity PWord
wordParser excludes gs = some $
    (GBoundary <$ single '#')
    <|> choice (fmap GMulti . chunk <$> gs)
    <|> (GMulti . pure <$> satisfy (not . exclude))
  where
    exclude = (||) <$> isSpace <*> (`elem` excludes)

-- | Megaparsec parser for 'Component's. Similarly to 'wordParser',
-- usually it’s easier to use 'tokeniseWords' instead.
componentsParser
    :: ParsecT Void String Identity a
    -> ParsecT Void String Identity [Component a]
componentsParser p = many $
    (Separator <$> takeWhile1P Nothing isSpace) <|>
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

sortByDescendingLength :: [[a]] -> [[a]]
sortByDescendingLength = sortBy (compare `on` Down . length)

-- | Tokenise a 'String' input word into a 'PWord' by splitting it up
-- into t'Grapheme's. A list of available multigraphs is supplied as
-- the first argument.
--
-- Note that this tokeniser is greedy: if one of the given
-- multigraphs is a prefix of another, the tokeniser will prefer the
-- longest if possible. If there are no matching multigraphs starting
-- at a particular character in the 'String', 'tokeniseWord' will
-- treat that character as its own t'Grapheme'. For instance:
--
-- >>> tokeniseWord [] "cherish"
-- Right [GMulti "c",GMulti "h",GMulti "e",GMulti "r",GMulti "i",GMulti "s",GMulti "h"]
-- 
-- >>> tokeniseWord ["e","h","i","r","s","sh"] "cherish"
-- Right [GMulti "c",GMulti "h",GMulti "e",GMulti "r",GMulti "i",GMulti "sh"]
-- 
-- >>> tokeniseWord ["c","ch","e","h","i","r","s","sh"] "cherish"
-- Right [GMulti "ch",GMulti "e",GMulti "r",GMulti "i",GMulti "sh"]
tokeniseWord :: [String] -> String -> Either (ParseErrorBundle String Void) PWord
tokeniseWord (sortByDescendingLength -> gs) = parse (wordParser "[" gs) ""

-- | Given a list of available multigraphs, tokenise an input string
-- into a list of words and other 'Component's. This uses the same
-- tokenisation strategy as 'tokeniseWords', but also recognises
-- 'Gloss'es (in square brackets) and 'Separator's (in the form of
-- whitespace).
tokeniseWords :: [String] -> String -> Either (ParseErrorBundle String Void) [Component PWord]
tokeniseWords (sortByDescendingLength -> gs) =
        parse (componentsParser $ wordParser "[" gs) ""

-- | Given a function to convert 'Word's to strings, converts a list
-- of 'Component's to strings.
detokeniseWords' :: (a -> String) -> [Component a] -> String
detokeniseWords' f = concatMap $ \case
    Word gs -> f gs
    Separator w -> w
    Gloss g -> '[':(g ++ "]")

-- | Specialisation of 'detokeniseWords'' for 'PWord's, converting
-- words to strings using 'concatWithBoundary'.
detokeniseWords :: [Component PWord] -> String
detokeniseWords = detokeniseWords' concatWithBoundary

-- | Given a list of sound changes, extract the list of multigraphs
-- defined in the first categories declaration of the 'SoundChange's.
findFirstCategoriesDecl :: SoundChanges c [Grapheme] -> [String]
findFirstCategoriesDecl (DirectiveS gs:_) =
    mapMaybe
        (\case GBoundary -> Nothing; GMulti m -> Just m)
        gs
findFirstCategoriesDecl (_:ss) = findFirstCategoriesDecl ss
findFirstCategoriesDecl [] = []

-- | CPS'd form of 'findFirstCategoriesDecl'. Nice for doing things
-- like @'withFirstCategoriesDecl' 'tokeniseWords' changes words@ (to
-- tokenise using the graphemes from the first categories declaration)
-- and so on.
withFirstCategoriesDecl :: ([String] -> t) -> SoundChanges c [Grapheme] -> t
withFirstCategoriesDecl tok ss = tok (findFirstCategoriesDecl ss)
