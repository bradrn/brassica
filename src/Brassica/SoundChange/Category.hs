{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}

module Brassica.SoundChange.Category where

import Data.Coerce
import Data.List (intersect)
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as M
import Data.Containers.ListUtils (nubOrd)

-- | Type-level tag for 'Category'. When parsing a category definition
-- from a string, usually categories will refer to other
-- categories. This is the 'Unexpanded' state. Once 'Expanded', these
-- references will have been inlined, and the category no longer
-- depends on other categories.
data CategoryState = Unexpanded | Expanded

-- | A set of values which are treated the same way in a sound
-- change. Note that Brassica makes no distinction between ad-hoc
-- categories and predefined categories beyond the parser; the latter
-- is merely syntax sugar for the former, and both are represented
-- using the same 'Category' type.
data Category (s :: CategoryState) a
    = Empty
    -- ^ The empty category (@[]@ in Brassica syntax)
    | Node a
    -- ^ A single value (@[a]@)
    | UnionOf [Category s a]
    -- ^ The union of multiple categories (@[Ca Cb Cc]@)
    | Intersect (Category s a) (Category s a)
    -- ^ The intersection of two categories (@[Ca +Cb]@)
    | Subtract (Category s a) (Category s a)
    -- ^ The second category subtracted from the first (@[Ca -Cb]@)
    deriving (Show, Eq, Ord, Functor)

-- | A map from category values to other categories. This type
-- expresses a set of categories which can be used to resolve
-- cross-references from other categories.
type Categories a = M.Map a (Category 'Expanded a)

-- | Lookup a category by name. Equivalent to
-- 'Data.Map.Strict.lookup'.
lookup :: Ord a => a -> Categories a -> Maybe (Category 'Expanded a)
lookup = M.lookup

-- | Map a function over 'Categories'.
mapCategories :: Ord b => (a -> b) -> Categories a -> Categories b
mapCategories f = M.map (fmap f) . M.mapKeys f

-- | Given a list of values, return a 'Category' which matches only
-- those values. (This is a simple wrapper around 'Node' and
-- 'UnionOf'.)
categorise :: Ord a => [a] -> Category 'Expanded a
categorise = UnionOf . fmap Node

-- | Expand an 'Unexpanded' category by inlining its references. The
-- references should only be to categories in the given 'Categories'.
expand :: Ord a => Categories a -> Category 'Unexpanded a -> Category 'Expanded a
expand _  Empty           = Empty
expand cs n@(Node a)      = fromMaybe (coerce n) $ M.lookup a cs
expand cs (UnionOf u)     = UnionOf $ expand cs <$> u
expand cs (Intersect a b) = Intersect (expand cs a) (expand cs b)
expand cs (Subtract a b)  = Subtract  (expand cs a) (expand cs b)

-- | Given an 'Expanded' category, return the list of values which it
-- matches.
bake :: Eq a => Category 'Expanded a -> [a]
bake Empty           = []
bake (Node    a)     = [a]
bake (UnionOf u)     = concatMap bake u
bake (Intersect a b) = bake a `intersect` bake b
bake (Subtract  a b) = bake a `difference` bake b
  where
    difference l m = filter (not . (`elem` m)) l

-- | Returns a list of all values referenced in a 'Categories'. This
-- includes all values even if they do not match a category:
-- e.g. given @[a b -a]@ this will return @["a","b"]@, not @["b"]@.
values :: Ord a => Categories a -> [a]
values = nubOrd . concatMap go . M.elems
  where
    go Empty           = []
    go (Node    a)     = [a]
    go (UnionOf u)     = concatMap go u
    go (Intersect a b) = go a ++ go b
    go (Subtract  a b) = go a ++ go b
