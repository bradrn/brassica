{-# LANGUAGE DeriveTraversable #-}

module MultiZipper
       ( MultiZipper
       -- * Conversion
       , fromListStart
       , toList
       -- * Querying
       , curPos
       , atStart
       , atEnd
       , atBoundary
       , value
       , valueN
       -- * Movement
       , move
       , fwd
       , bwd
       , seek
       , toBeginning
       , toEnd
       -- * Modification
       , tag
       , untag
       , modifyBetween
       , extend
       ) where

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
-- it makes other algorithms simpler. For instance, this lets
-- functions processing a 'MultiZipper' to process a portion of the
-- 'MultiZipper' and then move to the next element immediately after
-- the processed portion, allowing another function to be run to
-- process the next part of the 'MultiZipper'.)
data MultiZipper t a = MultiZipper [a] Int (M.Map t Int)
    deriving (Show, Functor, Foldable, Traversable)

-- | Convert a list to a 'MultiZipper' positioned at the start of that
-- list.
fromListStart :: [a] -> MultiZipper t a
fromListStart as = MultiZipper as 0 M.empty

-- | Get the list stored in a 'MultiZipper'.
toList :: MultiZipper t a -> [a]
toList (MultiZipper as _ _) = as

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
atEnd (MultiZipper as pos _) = pos > (length as - 1)

-- | Determine whether the 'MultiZipper' is positioned at the start or
-- end of its list.
atBoundary :: MultiZipper t a -> Bool
atBoundary = (||) <$> atStart <*> atEnd

-- | The element at the current position of the 'MultiZipper'. Returns
-- 'Nothing' if the 'MultiZipper' is positioned ‘at the end of the
-- list’ (recall this actually means that the 'MultiZipper' is
-- positioned /after/ the last element of its list).
value :: MultiZipper t a -> Maybe a
value (MultiZipper as pos _) =
    if pos >= length as
    then Nothing
    else Just $ as !! pos

-- | @valueN n mz@ returns the next @n@ elements of @mz@ starting from
-- the current position, as well as returning a new 'MultiZipper'
-- positioned past the end of those @n@ elements. (So running
-- @valueN m@ and then @valueN n@ would return the next @m+n@
-- elements.) Returns 'Nothing' if this would move the position of the
-- 'MultiZipper' past the end of the list.
valueN :: Int -> MultiZipper t a -> Maybe ([a], MultiZipper t a)
valueN i (MultiZipper as pos ts) =
    let pos' = pos + i in
        if pos' > length as || i < 0
        then Nothing
        else Just (take i $ drop pos as, MultiZipper as pos' ts)

locationOf :: Ord t => t -> MultiZipper t a -> Maybe Int
locationOf t (MultiZipper _ _ ts) = M.lookup t ts

-- These functions don't seem to be as useful as I thought they would,
-- but I'm keeping them here in case they do turn out to be useful at
-- some point
--
-- queryAt :: Ord t => Int -> MultiZipper t a -> [t]
-- queryAt i (MultiZipper _ _ ts) = M.keys $ M.filter (==i) ts
--
-- query :: Ord t => MultiZipper t a -> [t]
-- query mz@(MultiZipper _ pos _) = queryAt pos mz

seekIx :: Int -> MultiZipper t a -> Maybe (MultiZipper t a)
seekIx i (MultiZipper as _ ts) =
    if (i > length as) || (i < 0)
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

-- | Set a tag at the current position.
tag :: Ord t => t -> MultiZipper t a -> MultiZipper t a
tag t (MultiZipper as pos ts) = MultiZipper as pos $ M.insert t pos ts

-- | Remove all tags.
untag :: MultiZipper t a -> MultiZipper t a
untag (MultiZipper as pos _) = MultiZipper as pos M.empty

-- | Modify a 'MultiZipper' between the selected tags. Returns
-- 'Nothing' if a nonexistent tag is selected, else returns the
-- modified 'MultiZipper'.
modifyBetween :: Ord t
              => (t, t)
              -- ^ Selected tags. Note that the resulting interval
              -- will be [inclusive, exclusive).
              -> ([a] -> [a])
              -- ^ Function to modify designated interval.
              -> MultiZipper t a
              -> Maybe (MultiZipper t a)
modifyBetween (t1, t2) f mz@(MultiZipper as pos ts) = do
    (i1, i2) <- fmap correctOrder $ (,) <$> locationOf t1 mz <*> locationOf t2 mz
    let (before_t1, after_t1) = splitAt i1 as
        (cut_part, after_t2) = splitAt (i2-i1) after_t1
        insert = f cut_part
        dEnd = length insert - length cut_part
    return $ MultiZipper (before_t1 ++ insert ++ after_t2) pos (M.adjust (+dEnd) t2 ts)
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
    as' = fmap (\i -> f $ MultiZipper as i ts) [0 .. length as - 1]
