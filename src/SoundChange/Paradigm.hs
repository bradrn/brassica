{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module SoundChange.Paradigm
       ( Slot
       , Insertion(..)
       , Affix
       , Grammeme(..)
       , Feature(..)
       , Paradigm(..)
       , build
       ) where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)

type Slot = Int

data Insertion = Null | Adfix Slot String
    deriving (Show, Eq)

type Affix = [Insertion]

data Grammeme = Concrete Affix | Abstract String
    deriving (Show, Eq)

data Feature = Feature (Maybe String) [Grammeme]
    deriving (Show, Eq)

data Paradigm = Paradigm [Feature] [([String], Affix)]
    deriving (Show, Eq)

build :: Paradigm -> [String] -> [String]
build p ws = ws >>= applyParadigm p

applyParadigm :: Paradigm -> String -> [String]
applyParadigm (Paradigm fs ms) w = applyTo w . expand ms . fmap snd <$> combinations fs

combinations :: [Feature] -> [[(Maybe String, Grammeme)]]
combinations = traverse $ \(Feature n gs) -> (n,) <$> gs

expand :: [([String], Affix)] -> [Grammeme] -> [Insertion]
expand ms = concat . ((++) <$> concretes <*> (replace . abstracts))
  where
    concretes :: [Grammeme] -> [Affix]
    concretes = mapMaybe $ \case
        Concrete affix -> Just affix
        Abstract _ -> Nothing

    abstracts :: [Grammeme] -> [String]
    abstracts = mapMaybe $ \case
        Concrete _ -> Nothing
        Abstract g -> Just g

    replace :: [String] -> [Affix]
    replace gs = do
        (condition, replacement) <- ms
        if gs `subsetOf` condition
           then return replacement
           else []

    xs `subsetOf` ys = all (`elem` ys) xs

applyTo :: String -> [Insertion] -> String
applyTo w = appendToWord . span ((<0) . fst) . sortOn fst . mapMaybe getAdfix
  where
    getAdfix (Adfix s c) = Just (s,c)
    getAdfix Null = Nothing

    appendToWord (pres, sufs) = (pres >>= snd) ++ w ++ (sufs >>= snd)
