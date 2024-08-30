{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Brassica.SoundChange.Types
       (
       -- * Words and graphemes
         Grapheme(..)
       , PWord
       , addBoundaries
       , removeBoundaries
       , concatWithBoundary
       -- * Lexemes
       , Lexeme(..)
       , pattern Boundary
       , LexemeType(..)
       , generalise
       -- * Categories
       , mapCategory
       , mapCategoryA
       , Expanded(..)
       , generaliseExpanded
       -- * Rules
       , Rule(..)
       , Environment
       , Direction(..)
       , Sporadicity(..)
       , Flags(..)
       , defFlags
       -- * Statements
       , Filter(..)
       , Statement(..)
       , plaintext'
       , SoundChanges
       -- * Directives
       , CategoryModification(..)
       , CategorySpec(..)
       , FeatureSpec(..)
       , CategoryDefinition(..)
       , Directive(..)
       ) where

import Control.DeepSeq (NFData(..))
import Data.String (IsString(..))
import GHC.Generics (Generic)
import GHC.OldList (dropWhileEnd)

-- | The type of graphemes within a word.
data Grapheme
    = GMulti [Char]  -- ^ A multigraph: for instance @GMulti "a", GMulti "ch", GMulti "c̓" :: t'Grapheme'@.
    | GBoundary      -- ^ A non-letter element representing a word boundary which sound changes can manipulate
    deriving (Eq, Ord, Show, Generic, NFData)

instance IsString Grapheme where
    fromString = GMulti

-- | A word (or a subsequence of one) can be viewed as a list of
-- @Grapheme@s: e.g. Portuguese "filha" becomes
-- @["f", "i", "lh", "a"] :: 'PWord'@.
--
-- (The name 'PWord' is from ‘phonological word’, these being what a
-- SCA typically manipulates; this name was chosen to avoid a clash
-- with @Prelude.'Prelude.Word'@.)
type PWord = [Grapheme]

-- Add a 'GBoundary' at the beginning and end of the 'PWord'.
addBoundaries :: PWord -> PWord
addBoundaries w = GBoundary : w ++ [GBoundary]

-- Remove 'GBoundary's from the beginning and end of the 'PWord'.
removeBoundaries :: PWord -> PWord
removeBoundaries = dropWhile (==GBoundary) . dropWhileEnd (==GBoundary)

-- | Render a 'PWord' as a 'String'. Very much like 'concat', but
-- treating 'GBoundary's specially. Word-external boundaries are
-- deleted, while word-internal boundaries are converted to @"#"@.
concatWithBoundary :: PWord -> String
concatWithBoundary = go . removeBoundaries
  where
    go = concatMap $ \case
        GMulti g -> g
        GBoundary -> "#"

-- | The part of a 'Rule' in which a 'Lexeme' may occur: in a matched
-- part (target or environment), in replacement, or in either of
-- those.
data LexemeType = Matched | Replacement | AnyPart

-- | A 'Lexeme' is the smallest part of a sound change. Both matches
-- and replacements are made up of 'Lexeme's: the phantom type
-- variable @a@ specifies where each different variety of 'Lexeme' may
-- occur. 'Lexeme's are also parameterised by their category type,
-- which may be 'Expanded' or something else.
data Lexeme category (a :: LexemeType) where
    -- | In Brassica sound-change syntax, one or more letters without intervening whitespace,
    -- or a word boundary specified as @#@
    Grapheme :: Grapheme -> Lexeme category a
    -- | In Brassica sound-change syntax, delimited by square brackets
    Category :: category a -> Lexeme category a
    -- | In Brassica sound-change syntax, delimited by parentheses
    Optional :: [Lexeme category a] -> Lexeme category a
    -- | In Brassica sound-change syntax, specified as @\@
    Metathesis :: Lexeme category 'Replacement
    -- | In Brassica sound-change syntax, specified as @>@
    Geminate :: Lexeme category a
    -- | In Brassica sound-change syntax, specified as @^@ before another 'Lexeme'
    Wildcard :: Lexeme category a -> Lexeme category a
    -- | In Brassica sound-change syntax, specified as @*@ after another 'Lexeme'
    Kleene   :: Lexeme category a -> Lexeme category a
    -- | In Brassica sound-change syntax, specified as @~@
    Discard  :: Lexeme category 'Replacement
    -- | In Brassica sound-change syntax, specified as \@i before a category
    Backreference :: Int -> category a -> Lexeme category a
    -- | In Brassica sound-change syntax, specified as \@? before a category
    Multiple :: category 'Replacement -> Lexeme category 'Replacement
    -- | In Brassica sound-change syntax, specified as
    -- @$name#id(a~a′~a″ b~b′~b″ …)@ after another 'Lexeme'
    Feature :: String -> Maybe String -> [[String]] -> Lexeme category a -> Lexeme category a
    -- | Special lexeme for internal use: acts as a non-capturing
    -- category in target/environment, and as 'Grapheme' in
    -- replacement, in each case surrounded by a 'Feature'
    Autosegment :: String -> [[String]] -> [String] -> Lexeme category a

mapCategory :: (forall x. c x -> c' x) -> Lexeme c a -> Lexeme c' a
mapCategory _ (Grapheme g) = Grapheme g
mapCategory f (Category c) = Category (f c)
mapCategory f (Optional ls) = Optional (mapCategory f <$> ls)
mapCategory _ Metathesis = Metathesis
mapCategory _ Geminate = Geminate
mapCategory f (Wildcard l) = Wildcard (mapCategory f l)
mapCategory f (Kleene l) = Kleene (mapCategory f l)
mapCategory _ Discard = Discard
mapCategory f (Backreference i c) = Backreference i (f c)
mapCategory f (Multiple c) = Multiple (f c)
mapCategory f (Feature n i kvs l) = Feature n i kvs $ mapCategory f l
mapCategory _ (Autosegment n kvs gs) = Autosegment n kvs gs

mapCategoryA
    :: Applicative t
    => (forall x. c x -> t (c' x))
    -> Lexeme c a
    -> t (Lexeme c' a)
mapCategoryA _ (Grapheme g) = pure $ Grapheme g
mapCategoryA f (Category c) = Category <$> f c
mapCategoryA f (Optional ls) = Optional <$> traverse (mapCategoryA f) ls
mapCategoryA _ Metathesis = pure Metathesis
mapCategoryA _ Geminate = pure Geminate
mapCategoryA f (Wildcard l) = Wildcard <$> mapCategoryA f l
mapCategoryA f (Kleene l) = Kleene <$> mapCategoryA f l
mapCategoryA _ Discard = pure Discard
mapCategoryA f (Backreference i c) = Backreference i <$> f c
mapCategoryA f (Multiple c) = Multiple <$> f c
mapCategoryA f (Feature n i kvs l) = Feature n i kvs <$> mapCategoryA f l
mapCategoryA _ (Autosegment n kvs gs) = pure $ Autosegment n kvs gs

-- | The type of a category after expansion.
newtype Expanded a = FromElements { elements :: [Either Grapheme [Lexeme Expanded a]] }
    deriving (Eq, Ord, Show, Generic, NFData)

instance Semigroup (Expanded a) where
    (FromElements es) <> (FromElements es') = FromElements (es <> es')

instance Monoid (Expanded a) where
    mempty = FromElements []

generalise :: (c 'AnyPart -> c a) -> Lexeme c 'AnyPart -> Lexeme c a
generalise _ (Grapheme g) = Grapheme g
generalise f (Category es) = Category $ f es
generalise f (Optional ls) = Optional $ generalise f <$> ls
generalise _ Geminate = Geminate
generalise f (Backreference i es) = Backreference i $ f es
generalise f (Wildcard l) = Wildcard $ generalise f l
generalise f (Kleene l) = Kleene $ generalise f l
generalise f (Feature n i kvs l) = Feature n i kvs $ generalise f l
generalise _ (Autosegment n kvs gs) = Autosegment n kvs gs

generaliseExpanded :: Expanded 'AnyPart -> Expanded a
generaliseExpanded = FromElements . (fmap.fmap.fmap) (generalise generaliseExpanded) . elements

-- | A 'Lexeme' matching a single word boundary, specified as @#@ in Brassica syntax.
pattern Boundary :: Lexeme c a
pattern Boundary = Grapheme GBoundary

deriving instance (forall x. Show (c x)) => Show (Lexeme c a)
deriving instance (forall x. Eq (c x)) => Eq (Lexeme c a)
deriving instance (forall x. Ord (c x)) => Ord (Lexeme c a)

instance (forall x. NFData (c x)) => NFData (Lexeme c a) where
    rnf (Grapheme g) = rnf g
    rnf (Category cs) = rnf cs
    rnf (Optional ls) = rnf ls
    rnf Metathesis = ()
    rnf Geminate = ()
    rnf (Wildcard l) = rnf l
    rnf (Kleene l) = rnf l
    rnf Discard = ()
    rnf (Backreference i l) = seq i $ rnf l
    rnf (Multiple l) = rnf l
    rnf (Feature n i kvs l) = seq (rnf l) $ seq (rnf n) $ seq (rnf i) $ rnf kvs
    rnf (Autosegment n kvs gs) = seq (rnf n) $ seq (rnf kvs) $ rnf gs

-- | An 'Environment' is a tuple of @(before, after)@ components,
-- corresponding to a ‘/ before _ after’ component of a sound change.
--
-- Note that an empty environment is just @([], [])@.
type Environment c = ([Lexeme c 'Matched], [Lexeme c 'Matched])

-- | Specifies application direction of rule — either left-to-right or right-to-left.
data Direction = LTR | RTL
    deriving (Eq, Show, Generic, NFData)

-- | Specifies how regularly a rule should be applied.
data Sporadicity
    = ApplyAlways
    -- ^ Always apply the rule
    | PerWord
    -- ^ Apply sporadically, either to the whole word or to none of the word
    | PerApplication
    -- ^ Apply sporadically, at each application site
    deriving (Eq, Show, Generic, NFData)

-- | Flags which can be enabled, disabled or altered on a 'Rule' to
-- change how it is applied.
data Flags = Flags
  { highlightChanges :: Bool
  , applyDirection   :: Direction
  , applyOnceOnly    :: Bool
  , sporadic         :: Sporadicity
  , nonOverlappingTarget :: Bool
  } deriving (Show, Generic, NFData)

-- | A default selection of flags which are appropriate for most
-- rules:
--
-- @
-- 'defFlags' = 'Flags'
--     { 'highlightChanges' = 'True'
--     , 'applyDirection' = 'LTR'
--     , 'applyOnceOnly' = 'False'
--     , 'sporadic' = 'False'
--     }
-- @
--
-- That is: highlight changes, apply from left to right, apply
-- repeatedly, and don’t apply sporadically.
defFlags :: Flags
defFlags = Flags
    { highlightChanges = True
    , applyDirection = LTR
    , applyOnceOnly = False
    , sporadic = ApplyAlways
    , nonOverlappingTarget = False
    }

-- | A single sound change rule: in Brassica sound-change syntax with all elements specified,
-- @-flags target / replacement \/ environment1 | environment2 | … \/ exception@.
-- (And usually the 'plaintext' of the rule will contain a 'String' resembling that pattern.)
data Rule c = Rule
  { target      :: [Lexeme c 'Matched]
  , replacement :: [Lexeme c 'Replacement]
  , environment :: [Environment c]
  , exception   :: Maybe (Environment c)
  , flags       :: Flags
  , plaintext   :: String
  } deriving (Generic)

deriving instance (forall a. Show (c a)) => Show (Rule c)
deriving instance (forall a. NFData (c a)) => NFData (Rule c)

-- | A filter, constraining the output to not match the given elements.
-- (The 'String' is the plaintext, as with 'Rule'.)
data Filter c = Filter String [Lexeme c 'Matched]
    deriving (Generic)

deriving instance (forall a. Show (c a)) => Show (Filter c)
deriving instance (forall a. NFData (c a)) => NFData (Filter c)

-- | A 'Statement' can be a single sound change rule, a filter,
-- or a directive (e.g. category definition).
data Statement c decl
    = RuleS (Rule c)
    | FilterS (Filter c)
    | ReportS
    | DirectiveS decl
    deriving (Generic)

deriving instance (forall a. Show (c a), Show decl) => Show (Statement c decl)
deriving instance (forall a. NFData (c a), NFData decl) => NFData (Statement c decl)

-- | A simple wrapper around 'plaintext' for 'Statement's. Returns
-- @"<directive>"@ for all 'DirectiveS' inputs.
plaintext' :: Statement c decl -> String
plaintext' (RuleS r) = plaintext r
plaintext' (FilterS (Filter p _)) = p
plaintext' ReportS = "intermediate result"
plaintext' (DirectiveS _) = "<directive>"

-- | A set of 'SoundChanges' is simply a list of 'Statement's.
type SoundChanges c decl = [Statement c decl]

-- | The individual operations used to construct a category in
-- Brassica sound-change syntax.
data CategoryModification = Union | Intersect | Subtract
    deriving (Show, Eq, Ord, Generic, NFData)

-- | The specification of a category in Brassica sound-change syntax.
data CategorySpec a
    = CategorySpec [(CategoryModification, Either Grapheme [Lexeme CategorySpec a])]
    | MustInline String  -- ^ A single grapheme assumed to have been specified earlier as a category
    deriving (Show, Eq, Ord, Generic, NFData)

-- | The specification of a suprasegmental feature in Brassica
-- sound-change syntax.
data FeatureSpec = FeatureSpec
    { featureBaseName :: Maybe String
    , featureBaseValues :: CategorySpec 'AnyPart
    , featureDerived :: [(String, CategorySpec 'AnyPart)]
    }
    deriving (Show, Eq, Ord, Generic, NFData)

-- | A definition of a new category, either directly or via features.
data CategoryDefinition
    = DefineCategory String (CategorySpec 'AnyPart)
    | DefineFeature FeatureSpec
    | DefineAuto String
    deriving (Show, Eq, Ord, Generic, NFData)

-- | A directive used in Brassica sound-change syntax: anything which
-- is not actually a sound change

data Directive
    = Categories Bool Bool [CategoryDefinition]
      -- ^ @categories … end@: first 'Bool' for @new@,
      -- second for @noreplace@
    | ExtraGraphemes [String]
      -- ^ @extra …@
    deriving (Show, Eq, Ord, Generic, NFData)
