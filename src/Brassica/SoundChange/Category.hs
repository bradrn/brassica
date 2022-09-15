{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}

module Brassica.SoundChange.Category
       (
       -- * Category construction
         Category(..)
       , CategoryState(..)
       , categorise
       -- * Category expansion
       , Categories
       , Brassica.SoundChange.Category.lookup
       , mapCategories
       , expand
       -- * Obtaining values
       , bake
       , values
       ) where

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

-- | A set of values (usually representing phonemes) which behave the
-- same way in a sound change. A 'Category' is constructed using the
-- set operations supplied as constructors, possibly referencing other
-- 'Category's; these references can then be 'expand'ed, allowing the
-- 'Category' to be 'bake'd to a list of matching values.
--
-- Note that Brassica makes no distinction between ad-hoc categories
-- and predefined categories beyond the sound change parser; the
-- latter is merely syntax sugar for the former, and both are
-- represented using the same 'Category' type. In practise this is not
-- usually a problem, since 'Category's are still quite convenient to
-- construct manually.
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

-- | A map from names to the (expanded) categories they
-- reference. Used to resolve cross-references between categories.
type Categories a = M.Map a (Category 'Expanded a)

-- | @Data.Map.Strict.'Data.Map.Strict.lookup'@, specialised to 'Categories'.
lookup :: Ord a => a -> Categories a -> Maybe (Category 'Expanded a)
lookup = M.lookup

-- | Map a function over all the values in a set of 'Categories'.
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

-- | Returns a list of every value mentioned in a set of
-- 'Categories'. This includes all values, even those which are
-- 'Intersect'ed or 'Subtract'ed out: e.g. given 'Categories'
-- including @[a b -a]@, this will return a list including
-- @["a","b"]@, not just @["b"]@.
values :: Ord a => Categories a -> [a]
values = nubOrd . concatMap go . M.elems
  where
    go Empty           = []
    go (Node    a)     = [a]
    go (UnionOf u)     = concatMap go u
    go (Intersect a b) = go a ++ go b
    go (Subtract  a b) = go a ++ go b
