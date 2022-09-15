{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brassica.Paradigm.Types where

import Data.String (IsString)

-- | Represents a single morphophonological process: currently, either
-- prefixation or suffixation of a 'String'. The 'Int' gives the
-- distance of the affix from the root.
data Process
    = Prefix Int String
    | Suffix Int String
    deriving (Show, Eq)

-- | A single affix (using the term in a wide sense) can be thought of
-- as a list of morphophonological processes. For instance, the Berber
-- feminine circumfix /t-/…/-t/ might be represented as
-- @['Prefix' 1 "t", 'Suffix' 1 "t"] :: 'Affix'@.
type Affix = [Process]

-- | A 'Grammeme' represents one value of a grammatical feature: for
-- instance past, or dual. This can be realised as a 'Concrete' affix,
-- or can be left 'Abstract' so that it can be encoded in a cumulative
-- morph or similar.
--
-- (The name is from Wikipedia; it doesn’t seem widely-used, but I can
-- find no better for this concept.)
data Grammeme = Concrete Affix | Abstract AbstractGrammeme
    deriving (Show, Eq)

-- | An abstract identifier for a 'Grammeme'.
newtype AbstractGrammeme = AbstractGrammeme String
    deriving stock (Show, Eq)
    deriving newtype (IsString)

-- | A condition which must be satisfied before including a 'Feature'
-- in a word. 
data Condition
    = Always
    -- ^ Condition which is always satisfied
    | Is FeatureName Grammeme
    -- ^ Satisfied when the specified feature (identified by its name)
    -- has been assigned to the specified 'Grammeme'
    | Not FeatureName Grammeme
    -- ^ Satisfied when the specified feature has /not/ been assigned
    -- to the specified 'Grammeme'
    deriving (Show, Eq)

-- | A grammatical feature, which may be realised by one of a
-- selection of 'Grammeme's. A feature may be given a descriptive
-- name, as well as a condition which must be satisfied for the
-- 'Feature' to be included in a word.
data Feature = Feature Condition (Maybe FeatureName) [Grammeme]
    deriving (Show, Eq)

newtype FeatureName = FeatureName String
    deriving stock (Show, Eq)
    deriving newtype (IsString)

-- | Each statement in a paradigm description specifies either a new
-- 'Feature', or a new mapping from a set of abstract grammemes to
-- their realisation.
data Statement = NewFeature Feature | NewMapping [AbstractGrammeme] Affix
    deriving (Show, Eq)

-- | A paradigm is specified as a list of 'Statement's. The list is
-- basically big-endian, in that the slowest-varying feature should be
-- listed first. (So if e.g. tense is listed first, then first all
-- words of tense 1 are listed, next all words of tense 2 are listed,
-- and so on.)
type Paradigm = [Statement]
