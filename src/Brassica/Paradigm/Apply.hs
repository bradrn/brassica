{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Brassica.Paradigm.Apply
       ( ResultsTree(..)
       , applyParadigm
       , formatNested
       , depth
       ) where

import Brassica.Paradigm.Types

import Data.Functor ((<&>))
import Data.List (sortOn, foldl', intercalate)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(Down))

-- | Results from applying a 'Paradigm' to a word, represented as a tree
-- containing each possible choice of 'Grammeme's.
data ResultsTree a = Node [ResultsTree a] | Result a
    deriving (Show, Functor, Foldable)

addLevel :: (a -> [a]) -> ResultsTree a -> ResultsTree a
addLevel f (Result r) = Node $ Result <$> f r
addLevel f (Node rs) = Node $ addLevel f <$> rs

-- | Depth of a 'ResultsTree': i.e. how many levels of 'Node's it
-- contains.
depth :: ResultsTree a -> Int
depth (Node ts) = maximum $ (1+) . depth <$> ts
depth (Result _) = 0

-- | Formats a 'ResultsTree' in a nested way, where the lowest-level
-- elements are separated by one space, the second-lowest are
-- separated by one newline, the third-lowest by two newlines, and so
-- on.
formatNested :: (a -> String) -> ResultsTree a -> String
formatNested f = snd . go
  where
    go (Result a) = (0, f a)
    go (Node rts) =
        let (depths, formatted) = unzip $ go <$> rts
            depth' = maximum depths
            separator =
                if depth' == 0
                then " "
                else replicate depth' '\n'
        in (1+depth', intercalate separator formatted)

-- | Apply the given 'Paradigm' to a root, to produce all possible
-- derived forms.
applyParadigm :: Paradigm -> String -> ResultsTree String
applyParadigm p w =
    let fs = mapMaybe getFeature p
        ms = mapMaybe getMapping p
    in applyTo w . expand ms <$> combinations fs
  where
    getFeature (NewFeature f) = Just f
    getFeature _ = Nothing

    getMapping (NewMapping k v) = Just (k,v)
    getMapping _ = Nothing

combinations :: [Feature] -> ResultsTree [Grammeme]
combinations =
    (fmap.fmap) snd . foldl' go (Result [])
  where
    addFeature
        :: Feature
        -> [(Maybe FeatureName, Grammeme)]
        -> [[(Maybe FeatureName, Grammeme)]]
    addFeature (Feature c n gs) acc
        | satisfied (flip lookup acc . Just) c
        = gs <&> \g -> (n, g):acc
        | otherwise
        = [acc]

    go  :: ResultsTree [(Maybe FeatureName, Grammeme)]
        -> Feature
        -> ResultsTree [(Maybe FeatureName, Grammeme)]
    go rt f = addLevel (addFeature f) rt

    satisfied
        :: (FeatureName -> Maybe Grammeme)
        -> Condition
        -> Bool
    satisfied _ Always = True
    satisfied l (Is n g) = case l n of
        Just g' -> g == g'
        Nothing -> False
    satisfied l (Not n g) = case l n of
        Just g' -> g /= g'
        Nothing -> True

expand :: [([AbstractGrammeme], Affix)] -> [Grammeme] -> [Process]
expand ms = concat . ((++) <$> concretes <*> (replace . filterAbstract))
  where
    concretes :: [Grammeme] -> [Affix]
    concretes = mapMaybe $ \case
        Concrete affix -> Just affix
        Abstract _ -> Nothing

    filterAbstract :: [Grammeme] -> [AbstractGrammeme]
    filterAbstract = mapMaybe $ \case
        Concrete _ -> Nothing
        Abstract g -> Just g

    replace :: [AbstractGrammeme] -> [Affix]
    replace gs = do
        (condition, replacement) <- ms
        if condition `subsetOf` gs
           then return replacement
           else []

    xs `subsetOf` ys = all (`elem` ys) xs

applyTo :: String -> [Process] -> String
applyTo w is =
    let ps = concatMap snd $ sortOn (Down . fst) $ mapMaybe getPrefix is
        ss = concatMap snd $ sortOn         fst  $ mapMaybe getSuffix is
    in ps ++ w ++ ss
  where
    getPrefix (Prefix s i) = Just (s,i)
    getPrefix _ = Nothing

    getSuffix (Suffix s i) = Just (s,i)
    getSuffix _ = Nothing
