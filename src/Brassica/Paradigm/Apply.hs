{-# LANGUAGE LambdaCase    #-}

module Brassica.Paradigm.Apply (applyParadigm) where

import Brassica.Paradigm.Types

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(Down))

-- | Apply the given 'Paradigm' to a root, to produce all possible
-- derived forms.
applyParadigm :: Paradigm -> String -> [String]
applyParadigm p w =
    let fs = mapMaybe getFeature p
        ms = mapMaybe getMapping p
    in applyTo w . expand ms <$> combinations fs
  where
    getFeature (NewFeature f) = Just f
    getFeature _ = Nothing

    getMapping (NewMapping k v) = Just (k,v)
    getMapping _ = Nothing
      
combinations :: [Feature] -> [[Grammeme]]
combinations = go []
  where
    go :: [(Maybe FeatureName, Grammeme)] -> [Feature] -> [[Grammeme]]
    go acc [] = return $ snd <$> reverse acc
    go acc (Feature c n gs : fs) =
        if satisfied (flip lookup acc . Just) c
        then do
            g <- gs
            go ((n,g) : acc) fs
        else go acc fs

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
