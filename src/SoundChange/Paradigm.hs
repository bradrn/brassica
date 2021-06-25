{-# LANGUAGE LambdaCase    #-}

module SoundChange.Paradigm
       ( Process(..)
       , Affix
       , Grammeme(..)
       , Condition(..)
       , Feature(..)
       , Statement(..)
       , Paradigm
       , build
       ) where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(Down))

-- | Represents a single morphophonological process: either
-- prefixation or suffixation. The 'Int' argument represents distance
-- from the root.
data Process
    = Prefix Int String
    | Suffix Int String
    deriving (Show, Eq)

-- | A single affix (using the term in a wide sense, to include
-- circumfixes etc.) can be thought of as a list of morphophonological
-- processes, with the zero list representing a zero affix.
type Affix = [Process]

-- | A 'Grammeme' represents one value of a grammatical feature, for
-- instance past, or dual. This can either be a 'Concrete' affix, or
-- an 'Abstract' feature as realised in a cumulative morph or similar.
--
-- (The name is from Wikipedia; it doesn’t seem widely-used, but I can
-- find no better for this concept.)
data Grammeme = Concrete Affix | Abstract String
    deriving (Show, Eq)

-- | A condition which must be satisfied before including a 'Feature'
-- in a word. 
data Condition
    = Always
    -- ^ Condition which is always satisfied
    | Is String Grammeme
    -- ^ Satisfied when the specified feature (identified by its name)
    -- has been assigned to the specified 'Grammeme'
    | Not String Grammeme
    -- ^ Satisfied when the specified feature has /not/ been assigned
    -- to the specified 'Grammeme'
    deriving (Show, Eq)

-- | A grammatical feature, which may be realised by one of a
-- selection of 'Grammeme's. A feature may be given a descriptive
-- name and a condition which must be satisfied.
data Feature = Feature Condition (Maybe String) [Grammeme]
    deriving (Show, Eq)

-- | Each statement in a paradigm description specifies either a new
-- 'Feature', or a new mapping from a set of abstract grammemes to
-- their realisation.
data Statement = NewFeature Feature | NewMapping [String] Affix
    deriving (Show, Eq)

-- | A paradigm is specified as a list of 'Statements'. The list is
-- basically big-endian, in that the slowest-varying feature should be
-- listed first. (So if e.g. tense is listed first, then first all
-- words of tense 1 are listed, next all words of tense 2 are listed,
-- and so on.)
type Paradigm = [Statement]

build :: Paradigm -> [String] -> [String]
build p ws = ws >>= applyParadigm p

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
    go :: [(Maybe String,Grammeme)] -> [Feature] -> [[Grammeme]]
    go acc [] = return $ snd <$> reverse acc
    go acc (Feature c n gs : fs) =
        if satisfied (flip lookup acc . Just) c
        then do
            g <- gs
            go ((n,g) : acc) fs
        else go acc fs

    satisfied
        :: (String -> Maybe Grammeme)
        -> Condition
        -> Bool
    satisfied _ Always = True
    satisfied l (Is n g) = case l n of
        Just g' -> g == g'
        Nothing -> False
    satisfied l (Not n g) = case l n of
        Just g' -> g /= g'
        Nothing -> True

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
