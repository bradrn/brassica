{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brassica.Paradigm.Types where

import Data.String (IsString)

-- | Represents a single morphophonological process.
data Process
    = Prefix Int String  -- ^ Prefixation at some distance from the root
    | Suffix Int String  -- ^ Suffixation at some distance from the root
    | Stem Int           -- ^ Select stem by number
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

-- | A condition which must be satisfied for a 'Feature' to be
-- included in a word.
data Condition
    = Is FeatureName Grammeme
    -- ^ Satisfied when the specified feature has been assigned to the
    -- specified 'Grammeme'
    | Not FeatureName Grammeme
    -- ^ Satisfied when the specified feature has /not/ been assigned
    -- to the specified 'Grammeme'
    deriving (Show, Eq)

-- | An expression built up from 'Pred'icates, combined with logical
-- and ('All') or logical or ('Any').
data Expr a = Any [Expr a] | All [Expr a] | Pred a
    deriving (Show, Eq)

-- | A grammatical feature, which may be realised by one of a
-- selection of 'Grammeme's. A feature may be given a descriptive
-- name, as well as a condition which must be satisfied for the
-- 'Feature' to be included in a word.
data Feature = Feature (Expr Condition) (Maybe String) [Grammeme]
    deriving (Show, Eq)

-- | Name to identify a specific 'Feature', or the paradigm 'Class'.
data FeatureName = FeatureName String | Class
    deriving stock (Show, Eq)

-- | Each statement in a paradigm description specifies either a new
-- 'Feature', or a new mapping from a set of abstract grammemes to
-- their realisation.
data Statement = NewFeature Feature | NewMapping (Expr AbstractGrammeme) Affix
    deriving (Show, Eq)

-- | A paradigm is specified as a list of 'Statement's. The order is
-- reflected in the output 'Brassica.Paradigm.Apply.ResultsTree': the
-- root has one child for each grammeme in the first 'Feature', with
-- each child then having one child for each grammeme in the second
-- 'Feature', and so on.
type Paradigm = [Statement]
