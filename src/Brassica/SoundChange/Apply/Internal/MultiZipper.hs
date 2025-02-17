{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Brassica.SoundChange.Apply.Internal.MultiZipper
-- Copyright   : See LICENSE file
-- License     : BSD3
-- Maintainer  : Brad Neimann
--
-- __Warning:__ This module is __internal__, and does __not__ follow
-- the Package Versioning Policy. It may be useful for extending
-- Brassica, but be prepared to track development closely if you import
-- this module.
module Brassica.SoundChange.Apply.Internal.MultiZipper
       ( MultiZipper
       -- * Conversion
       , fromListStart
       , fromListPos
       , toList
       -- * Querying
       , curPos
       , atStart
       , atEnd
       , atBoundary
       , value
       , valueN
       , locationOf
       , yank
       -- * Movement
       , move
       , fwd
       , bwd
       , consume
       , seek
       , toBeginning
       , toEnd
       -- * Modification
       , insert
       , insertMany
       , reverseMZ
       , zap
       , tag
       , tagAt
       , query
       , untag
       , untagWhen
       , delete
       , extend
       , extend'
       ) where

import Control.Applicative (Alternative((<|>)))
import Data.Foldable (Foldable(foldl'))
import Data.Vector ((!?), (!))
import Data.Vector.Mutable (write)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

-- | A 'MultiZipper' is a list zipper (list+current index), with the
-- addition of ‘tags’ which can be assigned to indices in the
-- list. Any tag may be assigned to any index, with the restriction
-- that two different indices may not be tagged with the same
-- tag. This sort of data structure is useful for certain algorithms,
-- where it can be convenient to use tags to save positions in the
-- list and then return back to them later.
--
-- (One subtlety: unlike most list zipper implementations, a
-- 'MultiZipper' positioned at the ‘end’ of a list is actually at
-- positioned at the index one past the end of the list, rather than
-- at the last element of the list. Although this makes some functions
-- slightly more complex — most notably, 'value' becomes non-total —
-- it makes sound changes application easier to implement. In
-- particular, it means that functions processing a portion of a
-- 'MultiZipper' can finish by moving to the next element immediately
-- after the processed portion; any subsequent function will then
-- continue by processing the next part of the 'MultiZipper'.)
data MultiZipper t a = MultiZipper (V.Vector a) Int (M.Map t Int)
    deriving (Show, Functor, Foldable, Traversable)

-- | Convert a list to a 'MultiZipper' positioned at the start of that
-- list.
fromListStart :: [a] -> MultiZipper t a
fromListStart as = MultiZipper (V.fromList as) 0 M.empty

-- | Convert a list to a 'MultiZipper' at a specific position in the
-- list. Returns 'Nothing' if the index is invalid.
fromListPos :: [a] -> Int -> Maybe (MultiZipper t a)
fromListPos as pos =
    if invalid pos (length as)
    then Nothing
    else Just $ MultiZipper (V.fromList as) pos M.empty

-- | Get the list stored in a 'MultiZipper'.
toList :: MultiZipper t a -> [a]
toList (MultiZipper as _ _) = V.toList as

-- | Reverse the contents of a 'MultiZipper', ensuring its current
-- position and tags remain attatched to their elements.
reverseMZ :: MultiZipper t a -> MultiZipper t a
reverseMZ (MultiZipper as pos ts) =
    let l = length as
    in MultiZipper
        (V.reverse as)
        (l - pos)
        (M.map (l-) ts)

-- | The current position of the 'MultiZipper'.
curPos :: MultiZipper t a -> Int
curPos (MultiZipper _ pos _) = pos

-- | Determine whether the 'MultiZipper' is positioned at the start of
-- its list.
atStart :: MultiZipper t a -> Bool
atStart (MultiZipper _ pos _) = pos <= 0

-- | Determine whether the 'MultiZipper' is positioned at the end of
-- its list.
atEnd :: MultiZipper t a -> Bool
atEnd (MultiZipper as pos _) = pos >= length as

-- | Determine whether the 'MultiZipper' is positioned at the start or
-- end of its list.
atBoundary :: MultiZipper t a -> Bool
atBoundary = (||) <$> atStart <*> atEnd

-- | The element at the current position of the 'MultiZipper'. Returns
-- 'Nothing' if the 'MultiZipper' is positioned ‘at the end of the
-- list’ (recall this actually means that the 'MultiZipper' is
-- positioned /after/ the last element of its list).
value :: MultiZipper t a -> Maybe a
value (MultiZipper as pos _) = as !? pos

-- | @valueN n mz@ returns the next @n@ elements of @mz@ starting from
-- the current position, as well as returning a new 'MultiZipper'
-- positioned past the end of those @n@ elements. (So running
-- @valueN m@ and then @valueN n@ would return the next @m+n@
-- elements.) Returns 'Nothing' if this would move the position of the
-- 'MultiZipper' past the end of the list.
valueN :: Int -> MultiZipper t a -> Maybe ([a], MultiZipper t a)
valueN i (MultiZipper as pos ts) =
    let pos' = pos + i in
        if invalid pos' (V.length as) || i < 0
        then Nothing
        else Just (take i $ drop pos $ V.toList as, MultiZipper as pos' ts)

-- | Given a tag, return its position
locationOf :: Ord t => t -> MultiZipper t a -> Maybe Int
locationOf t (MultiZipper _ _ ts) = M.lookup t ts

-- | Get all tags at the current position
query :: Ord t => MultiZipper t a -> [t]
query (MultiZipper _ pos ts) = M.keys $ M.filter (==pos) ts

seekIx :: Int -> MultiZipper t a -> Maybe (MultiZipper t a)
seekIx i (MultiZipper as _ ts) =
    if invalid i (V.length as)
    then Nothing
    else Just (MultiZipper as i ts)

-- | @move n mz@ will move the position of @mz@ by @n@ forward (if
-- n>0) or by @-n@ backward (if n<0). Returns 'Nothing' if this would
-- cause the 'MultiZipper' to move after the end or before the
-- beginning of the list.
move :: Int -> MultiZipper t a -> Maybe (MultiZipper t a)
move s mz@(MultiZipper _ pos _) = seekIx (pos + s) mz

-- | Move one position forward if possible, otherwise return 'Nothing'.
fwd :: MultiZipper t a -> Maybe (MultiZipper t a)
fwd = move 1

-- | Move one position backwards if possible, otherwise return 'Nothing'.
bwd :: MultiZipper t a -> Maybe (MultiZipper t a)
bwd = move (-1)

-- | If possible, move one position forward, returning the value moved
-- over
consume :: MultiZipper t a -> Maybe (a, MultiZipper t a)
consume (MultiZipper as pos ts) =
    fmap (,MultiZipper as (pos+1) ts) (as!?pos)

-- | Move the 'MultiZipper' to be at the specified tag. Returns
-- 'Nothing' if that tag is not present.
seek :: Ord t => t -> MultiZipper t a -> Maybe (MultiZipper t a)
seek t (MultiZipper as _ ts) = case M.lookup t ts of
    Nothing  -> Nothing
    Just pos -> Just $ MultiZipper as pos ts

-- | Move to the beginning of the 'MultiZipper'.
toBeginning :: MultiZipper t a -> MultiZipper t a
toBeginning (MultiZipper as _ ts) = MultiZipper as 0 ts

-- | Move to the end of the 'MultiZipper'.
toEnd :: MultiZipper t a -> MultiZipper t a
toEnd (MultiZipper as _ ts) = MultiZipper as (length as) ts

-- | Find first element before point which returns 'Just' when
-- queried, if any, returning the result of the query function.
yank :: (a -> Maybe b) -> MultiZipper t a -> Maybe b
yank p mz = bwd mz >>= \mz' -> (value mz' >>= p) <|> yank p mz'

-- | Insert a new element at point and move forward by one position.
insert :: a -> MultiZipper t a -> MultiZipper t a
insert a (MultiZipper as pos ts) =
    case V.splitAt pos as of
        (as1, as2) -> MultiZipper
            (as1 V.++ V.cons a as2)
            (pos+1)
            (correctIxsFrom pos (+1) ts)

-- | Insert multiple elements at point and move after them. A simple
-- wrapper around 'insert'.
insertMany :: [a] -> MultiZipper t a -> MultiZipper t a
insertMany = flip $ foldl' $ flip insert

-- | Modify the first element before point to which the modification
-- function returns 'Just'.
zap :: (a -> Maybe a) -> MultiZipper t a -> MultiZipper t a
zap p = \mz@(MultiZipper as pos ts) -> case go as (pos-1) of
    Nothing  -> mz
    Just as' -> MultiZipper as' pos ts
  where
    go _ (-1) = Nothing
    go as pos
      | pos == length as = go as (pos-1)
      | otherwise = case p (as ! pos) of
        Nothing -> go as (pos-1)
        Just a' -> Just $ V.modify (\v -> write v pos a') as

-- | Set a tag at the current position.
tag :: Ord t => t -> MultiZipper t a -> MultiZipper t a
tag t (MultiZipper as pos ts) = MultiZipper as pos $ M.insert t pos ts

-- | Set a tag at a given position if possible, otherwise return 'Nothing'.
tagAt :: Ord t => t -> Int -> MultiZipper t a -> Maybe (MultiZipper t a)
tagAt t i (MultiZipper as pos ts) =
    if invalid i (length as)
    then Nothing
    else Just $ MultiZipper as pos $ M.insert t i ts

-- | Remove tags satisfying predicate
untagWhen :: (t -> Bool) -> MultiZipper t a -> MultiZipper t a
untagWhen p (MultiZipper as pos ts) = MultiZipper as pos $ snd $ M.partitionWithKey (flip $ const p) ts

-- | Remove all tags.
untag :: MultiZipper t a -> MultiZipper t a
untag (MultiZipper as pos _) = MultiZipper as pos M.empty

-- | Delete the portion of a 'MultiZipper' between the selected tags.
-- Returns 'Nothing' if a nonexistent tag is selected, else returns
-- the modified 'MultiZipper'.
delete
    :: Ord t
    => (t, t)
    -- ^ Selected tags. Note that the resulting interval
    -- will be [inclusive, exclusive).
    -> MultiZipper t a
    -> Maybe (MultiZipper t a)
delete (t1, t2) mz@(MultiZipper as pos ts) = do
    (i1, i2) <- fmap correctOrder $ (,) <$> locationOf t1 mz <*> locationOf t2 mz
    let (before_t1, after_t1) = V.splitAt i1 as
        (cut_part, after_t2) = V.splitAt (i2-i1) after_t1
        removed = length cut_part
        pos' = pos - removed
    return $ MultiZipper (before_t1 V.++ after_t2) pos' (correctIxsFrom i2 (subtract removed) ts)
  where
    correctOrder (m, n) = if m <= n then (m, n) else (n, m)

-- | Given a function to compute a value from a 'MultiZipper' starting
-- at a particular point, apply that function to all possible starting
-- points and collect the results. Tags are left unchanged.
--
-- (Note: this is really just the same @extend@ method as in the
-- @Comonad@ typeclass, although 'MultiZipper' wouldn’t be a lawful
-- comonad.)
extend :: (MultiZipper t a -> b) -> MultiZipper t a -> MultiZipper t b
extend f (MultiZipper as pos ts) = MultiZipper as' pos ts
  where
    as' = V.map (\i -> f $ MultiZipper as i ts) $ V.enumFromN 0 (length as)

-- | Like 'extend', but includes the end position of the zipper, thus
-- increasing the 'MultiZipper' length by one when called.
extend' :: (MultiZipper t a -> b) -> MultiZipper t a -> MultiZipper t b
extend' f (MultiZipper as pos ts) = MultiZipper as' pos ts
  where
    as' = V.map (\i -> f $ MultiZipper as i ts) $ V.enumFromN 0 (length as + 1)

-- Utility functions for checking and modifying indices in lists:
invalid :: Int -> Int -> Bool
invalid pos len = (pos < 0) || (pos > len)

correctIxsFrom :: Int -> (Int -> Int) -> M.Map t Int -> M.Map t Int
correctIxsFrom i f = M.map $ \pos -> if pos >= i then f pos else pos
