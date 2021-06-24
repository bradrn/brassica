{-# LANGUAGE LambdaCase    #-}

module SoundChange.Paradigm
       ( Process(..)
       , Affix
       , Grammeme(..)
       , Feature(..)
       , Paradigm(..)
       , build
       ) where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(Down))

-- | Represents a single morphophonological process: either
-- prefixation or suffixation. The 'Int' argument represents distance
-- from the root.
data Process
    = Null
    | Prefix Int String
    | Suffix Int String
    deriving (Show, Eq)

-- | A single affix (using the term in a wide sense, to include
-- circumfixes etc.) can be thought of as a list of morphophonological
-- processes.
type Affix = [Process]

-- | A 'Grammeme' represents one value of a grammatical feature, for
-- instance past, or dual. This can either be a 'Concrete' affix, or
-- an 'Abstract' feature as realised in a cumulative morph or similar.
--
-- (The name is from Wikipedia; it doesn’t seem widely-used, but I can
-- find no better for this concept.)
data Grammeme = Concrete Affix | Abstract String
    deriving (Show, Eq)

-- | A grammatical feature, which may be realised by one of a
-- selection of 'Grammeme's. A feature may be given a descriptive
-- name.
data Feature = Feature (Maybe String) [Grammeme]
    deriving (Show, Eq)

-- | A paradigm is primarily a list of 'Feature's. The list is
-- basically big-endian, in that the slowest-varying feature should be
-- listed first. (So if e.g. tense is listed first, then first all
-- words of tense 1 are listed, next all words of tense 2 are listed,
-- and so on.) The 'Paradigm' also includes a mapping from sets of
-- 'Abstract' grammemes to their cumulative 'Affix' realisation.
data Paradigm = Paradigm [Feature] [([String], Affix)]
    deriving (Show, Eq)

build :: Paradigm -> [String] -> [String]
build p ws = ws >>= applyParadigm p

applyParadigm :: Paradigm -> String -> [String]
applyParadigm (Paradigm fs ms) w = applyTo w . expand ms <$> combinations fs
  where
    combinations :: [Feature] -> [[Grammeme]]
    combinations = mapM $ \(Feature _ gs) -> gs

expand :: [([String], Affix)] -> [Grammeme] -> [Process]
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

applyTo :: String -> [Process] -> String
applyTo w is =
    let ps = concatMap snd $ sortOn         fst  $ mapMaybe getPrefix is
        ss = concatMap snd $ sortOn (Down . fst) $ mapMaybe getSuffix is
    in ps ++ w ++ ss
  where
    getPrefix (Prefix s i) = Just (s,i)
    getPrefix _ = Nothing

    getSuffix (Suffix s i) = Just (s,i)
    getSuffix _ = Nothing
