{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE KindSignatures #-}

module SoundChange.Category where

import Data.Coerce
import Data.List (intersect, (\\))
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as M
import Data.Containers.ListUtils (nubOrd)

data CategoryState = Unexpanded | Expanded

data Category (s :: CategoryState) a
    = Empty
    | Node a
    | UnionOf [Category s a]
    | Intersect (Category s a) (Category s a)
    | Subtract  (Category s a) (Category s a)
    deriving (Show, Eq, Ord, Functor)

type Categories a = M.Map a (Category 'Expanded a)

lookup :: Ord a => a -> Categories a -> Maybe (Category 'Expanded a)
lookup = M.lookup

mapCategories :: Ord b => (a -> b) -> Categories a -> Categories b
mapCategories f = M.map (fmap f) . M.mapKeys f

expand :: Ord a => Categories a -> Category 'Unexpanded a -> Category 'Expanded a
expand _  Empty           = Empty
expand cs n@(Node a)      = fromMaybe (coerce n) $ M.lookup a cs
expand cs (UnionOf u)     = UnionOf $ expand cs <$> u
expand cs (Intersect a b) = Intersect (expand cs a) (expand cs b)
expand cs (Subtract a b)  = Subtract  (expand cs a) (expand cs b)

bake :: Eq a => Category 'Expanded a -> [a]
bake Empty           = []
bake (Node    a)     = [a]
bake (UnionOf u)     = concatMap bake u
bake (Intersect a b) = bake a `intersect` bake b
bake (Subtract  a b) = bake a \\ bake b

values :: Ord a => Categories a -> [a]
values = nubOrd . concatMap go . M.elems
  where
    go Empty           = []
    go (Node    a)     = [a]
    go (UnionOf u)     = concatMap go u
    go (Intersect a b) = go a ++ go b
    go (Subtract  a b) = go a ++ go b
