{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
-- Module      : Brassica.SoundChange.Types
-- Copyright   : See LICENSE file
-- License     : BSD3
-- Maintainer  : Brad Neimann
--
-- This module contains the types used to represent sound changes and
-- words in Brassica. In brief:
--
--     * A set of 'SoundChanges' is composed of a list of elements
--
--     * Their most important elements are sound change 'Rule's
--
--     * Sound changes are composed of 'Lexeme's denoting parts of the
--       input and output words
--
--     * Each word is a sequence of t'Grapheme's
--
-- For more details on the syntax and semantics of sound changes,
-- refer to the [reference guide](https://github.com/bradrn/brassica/blob/v1.0.0/docs/Reference.md).
module Brassica.SoundChange.Types
       (
       -- * Words and graphemes
         Grapheme
       , PWord
       , addBoundaries
       , removeBoundaries
       , concatWithBoundary
       -- * Lexemes
       , Lexeme(..)
       , LexemeType(..)
       , generalise
       -- * Categories
       , mapCategory
       , mapCategoryA
       , CategoryElement
       , CategorySpec(..)
       , CategoryModification(..)
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
       -- * Directives [TODO find better name]
       , Directive(..)
       , CategoryDefinition(..)
       , FeatureSpec(..)
       , GraphemeList
       ) where

import Control.DeepSeq (NFData(..), deepseq)
import GHC.Generics (Generic)
import GHC.OldList (dropWhileEnd)

-- | The type of graphemes within a word. @"#"@ is taken to denote a
-- word boundary (whch is universally treated as a normal grapheme in
-- sound changes.)
type Grapheme = [Char]

-- | Brassica views a word, or a subsequence of one, as a list of
-- @Grapheme@s. For instance, Portuguese "filha" becomes
-- @["f", "i", "lh", "a"]@ when tokenised correctly.
--
-- (The name 'PWord' is from ‘phonological word’, these being what
-- sound changes typically manipulate. The name was chosen to avoid a
-- clash with @t'Word'@ from @base@.)
type PWord = [Grapheme]

-- | Add word boundaries (@"#"@) at the beginning and end of a 'PWord'.
addBoundaries :: PWord -> PWord
addBoundaries w = "#" : w ++ ["#"]

-- | Remove word boundaries (@"#"@) from the beginning and end of a 'PWord'.
removeBoundaries :: PWord -> PWord
removeBoundaries = dropWhile (=="#") . dropWhileEnd (=="#")

-- | Render a 'PWord' as a 'String': does 'removeBoundaries' then 'concat'.
--
-- Inverse of 'Brassica.SoundChange.Tokenise.tokeniseWord'.
concatWithBoundary :: PWord -> String
concatWithBoundary = concat . removeBoundaries

-- | The part of a 'Rule' in which a 'Lexeme' may occur.
data LexemeType
    = Matched      -- ^ In the target, environment or exception (‘matching’ position)
    | Replacement  -- ^ In the replacement only
    | AnyPart      -- ^ Not restricted to any one part

-- | Each part of a sound change is made up of a sequence of
-- 'Lexeme's. Each 'Lexeme' denotes part of an input or output word.
--
-- The first type variable @category@ is the type used to represent
-- categories within the sound change. This will usually be
-- 'CategorySpec' after parsing, or 'Expanded' after expansion.
--
-- The second type variable is phantom and represents the part of the
-- rule in which the lexeme is placed. Various lexemes are restricted
-- to 'Matched' or 'Replacement' positions respectively.
--
-- For details on the syntax and semantics of each kind of lexeme,
-- refer to the [reference guide](https://github.com/bradrn/brassica/blob/v1.0.0/docs/Reference.md).
data Lexeme category (a :: LexemeType) where
    Grapheme :: Grapheme -> Lexeme category a
    Category :: category a -> Lexeme category a
    -- | Written @%category@, matching-only
    GreedyCategory :: category 'Matched -> Lexeme category 'Matched
    -- | Written @(lexemes)@
    Optional :: [Lexeme category a] -> Lexeme category a
    -- | Written @%(lexemes)@, matching-only
    GreedyOptional :: [Lexeme category 'Matched] -> Lexeme category 'Matched
    -- | Written @\\@, replacement-only
    Metathesis :: Lexeme category 'Replacement
    -- | Written @>@
    Geminate :: Lexeme category a
    -- | Written @^lexeme@
    Wildcard :: Lexeme category a -> Lexeme category a
    -- | Written @lexeme*@
    Kleene   :: Lexeme category a -> Lexeme category a
    -- | Written @~@, replacement-only
    Discard  :: Lexeme category 'Replacement
    -- | Written @\@n category@ or @\@#id category@
    Backreference :: Either String Int -> category a -> Lexeme category a
    -- | Written @\@? category@
    Multiple :: category 'Replacement -> Lexeme category 'Replacement
    -- | Written @lexeme$Name@ or variations (see reference guide)
    Feature
        :: Bool                -- ^ 'True' iff the feature is negated
        -> String              -- ^ Feature name
        -> Maybe String        -- ^ Identifier if backreferenced, else 'Nothing'
        -> [[Grapheme]]        -- ^ List of correspondence sets
        -> Lexeme category a
        -> Lexeme category a
    -- | Not directly available in Brassica syntax, inserted in expansion
    Autosegment
        :: Grapheme            -- ^ Feature name
        -> [[Grapheme]]        -- ^ List of correspondence sets
        -> [Grapheme]          -- ^ Graphemes to be matched by this 'Autosegment'
        -> Lexeme category a

-- | Map a function over any categories in the given 'Lexeme'.
mapCategory :: (forall x. c x -> c' x) -> Lexeme c a -> Lexeme c' a
mapCategory _ (Grapheme g) = Grapheme g
mapCategory f (Category c) = Category (f c)
mapCategory f (GreedyCategory c) = GreedyCategory (f c)
mapCategory f (Optional ls) = Optional (mapCategory f <$> ls)
mapCategory f (GreedyOptional ls) = GreedyOptional (mapCategory f <$> ls)
mapCategory _ Metathesis = Metathesis
mapCategory _ Geminate = Geminate
mapCategory f (Wildcard l) = Wildcard (mapCategory f l)
mapCategory f (Kleene l) = Kleene (mapCategory f l)
mapCategory _ Discard = Discard
mapCategory f (Backreference i c) = Backreference i (f c)
mapCategory f (Multiple c) = Multiple (f c)
mapCategory f (Feature r n i kvs l) = Feature r n i kvs $ mapCategory f l
mapCategory _ (Autosegment n kvs gs) = Autosegment n kvs gs

-- | Like 'mapCategory', with an 'Applicative' effect.
mapCategoryA
    :: Applicative t
    => (forall x. c x -> t (c' x))
    -> Lexeme c a
    -> t (Lexeme c' a)
mapCategoryA _ (Grapheme g) = pure $ Grapheme g
mapCategoryA f (Category c) = Category <$> f c
mapCategoryA f (GreedyCategory c) = GreedyCategory <$> f c
mapCategoryA f (Optional ls) = Optional <$> traverse (mapCategoryA f) ls
mapCategoryA f (GreedyOptional ls) = GreedyOptional <$> traverse (mapCategoryA f) ls
mapCategoryA _ Metathesis = pure Metathesis
mapCategoryA _ Geminate = pure Geminate
mapCategoryA f (Wildcard l) = Wildcard <$> mapCategoryA f l
mapCategoryA f (Kleene l) = Kleene <$> mapCategoryA f l
mapCategoryA _ Discard = pure Discard
mapCategoryA f (Backreference i c) = Backreference i <$> f c
mapCategoryA f (Multiple c) = Multiple <$> f c
mapCategoryA f (Feature r n i kvs l) = Feature r n i kvs <$> mapCategoryA f l
mapCategoryA _ (Autosegment n kvs gs) = pure $ Autosegment n kvs gs

-- | The type of a category after expansion: a simple list of
-- 'CategoryElement's.
newtype Expanded a = FromElements { elements :: [CategoryElement Expanded a] }
    deriving (Eq, Ord, Show, Generic, NFData)

instance Semigroup (Expanded a) where
    (FromElements es) <> (FromElements es') = FromElements (es <> es')

instance Monoid (Expanded a) where
    mempty = FromElements []

-- | Generalise a @'Lexeme' c ''AnyPart'@ so it can be used in any
-- specific part of a sound change, given a way to similarly
-- generalise any categories it contains.
generalise :: (c 'AnyPart -> c a) -> Lexeme c 'AnyPart -> Lexeme c a
generalise _ (Grapheme g) = Grapheme g
generalise f (Category es) = Category $ f es
generalise f (Optional ls) = Optional $ generalise f <$> ls
generalise _ Geminate = Geminate
generalise f (Backreference i es) = Backreference i $ f es
generalise f (Wildcard l) = Wildcard $ generalise f l
generalise f (Kleene l) = Kleene $ generalise f l
generalise f (Feature r n i kvs l) = Feature r n i kvs $ generalise f l
generalise _ (Autosegment n kvs gs) = Autosegment n kvs gs

-- | Generalise an 'Expanded' category to be used in any part of a
-- sound change, similarly to 'generalise'.
generaliseExpanded :: Expanded 'AnyPart -> Expanded a
generaliseExpanded = FromElements . (fmap.fmap) (generalise generaliseExpanded) . elements

deriving instance (forall x. Show (c x)) => Show (Lexeme c a)
deriving instance (forall x. Eq (c x)) => Eq (Lexeme c a)
deriving instance (forall x. Ord (c x)) => Ord (Lexeme c a)

instance (forall x. NFData (c x)) => NFData (Lexeme c a) where
    rnf (Grapheme g) = rnf g
    rnf (Category cs) = rnf cs
    rnf (GreedyCategory cs) = rnf cs
    rnf (Optional ls) = rnf ls
    rnf (GreedyOptional ls) = rnf ls
    rnf Metathesis = ()
    rnf Geminate = ()
    rnf (Wildcard l) = rnf l
    rnf (Kleene l) = rnf l
    rnf Discard = ()
    rnf (Backreference i l) = i `deepseq` rnf l
    rnf (Multiple l) = rnf l
    rnf (Feature r n i kvs l) = r `deepseq` l `deepseq` n `deepseq` i `deepseq` rnf kvs
    rnf (Autosegment n kvs gs) = n `deepseq` kvs `deepseq` rnf gs

-- | An 'Environment' is a tuple of @(before, after)@ components,
-- corresponding to an environment or exception in a sound change:
-- @before _ after@.
--
-- (An empty environment is just @([], [])@.)
type Environment c = ([Lexeme c 'Matched], [Lexeme c 'Matched])

-- | Specifies application direction of rule: either left-to-right or right-to-left.
data Direction = LTR | RTL
    deriving (Eq, Show, Generic, NFData)

-- | Specifies how regularly a rule should be applied. A sporadic
-- rule will produce two or more results, preserving the input as one
-- of the outputs.
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
  { highlightChanges :: Bool         -- ^ Whether results from this sound change can be highlighted in a GUI
  , applyDirection   :: Direction    -- ^ Direction in which to apply the rule
  , applyOnceOnly    :: Bool         -- ^ Whether to apply the rule only once to a word
  , sporadic         :: Sporadicity  -- ^ Whether the rule should be applied sporadically, and if so, how
  , nonOverlappingTarget :: Bool     -- ^ Whether the rule should apply non-iteratively (avoiding environments which overlap with targets)
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
--     , 'nonOverlappingTarget' = 'False'
--     }
-- @
--
-- That is: apply repeatedly and iteratively from left to right,
-- non-sporadically, with the results available for highlighting.
defFlags :: Flags
defFlags = Flags
    { highlightChanges = True
    , applyDirection = LTR
    , applyOnceOnly = False
    , sporadic = ApplyAlways
    , nonOverlappingTarget = False
    }

-- | A single sound change rule.
--
-- In Brassica sound-change syntax with all elements specified, this would be
-- @-flags target / replacement \/ environment1 \/ environment2 \/ … \/ exception@.
data Rule c = Rule
  { target      :: [Lexeme c 'Matched]
  , replacement :: [Lexeme c 'Replacement]
  , environment :: [Environment c]
  , exception   :: Maybe (Environment c)
  , flags       :: Flags
  , plaintext   :: String  -- ^ Rule text before parsing (displayed e.g. for debugging purposes)
  } deriving (Generic)

deriving instance (forall a. Show (c a)) => Show (Rule c)
deriving instance (forall a. NFData (c a)) => NFData (Rule c)

-- | A filter, constraining the output to not match the given elements.
-- (The 'String' is the plaintext, as with 'Rule'.)
data Filter c = Filter String [Lexeme c 'Matched]
    deriving (Generic)

deriving instance (forall a. Show (c a)) => Show (Filter c)
deriving instance (forall a. NFData (c a)) => NFData (Filter c)

-- | A 'Statement' within a sound change file can be a single sound
-- change rule, a filter, or some other directive.
--
-- The directive depends on the current sound change phase. Usually it
-- will be 'Directive' after parsing, or 'GraphemeList' after
-- expansion.
data Statement c decl
    = RuleS (Rule c)        -- ^ Sound change rule
    | FilterS (Filter c)    -- ^ Filter
    | ReportS               -- ^ Report intermediate result
    | DirectiveS decl
    deriving (Generic)

deriving instance (forall a. Show (c a), Show decl) => Show (Statement c decl)
deriving instance (forall a. NFData (c a), NFData decl) => NFData (Statement c decl)

-- | A simple wrapper around 'plaintext' for 'Statement's. Returns
-- @"\<directive\>"@ for all 'DirectiveS' inputs.
plaintext' :: Statement c decl -> String
plaintext' (RuleS r) = plaintext r
plaintext' (FilterS (Filter p _)) = p
plaintext' ReportS = "intermediate result"
plaintext' (DirectiveS _) = "<directive>"

-- | A set of 'SoundChanges' is simply a list of 'Statement's.
type SoundChanges c decl = [Statement c decl]

-- | The individual operations used to construct a category in
-- Brassica sound-change syntax.
data CategoryModification
    = Union     -- ^ Written @[Category1 &Category2]@ or @[Category1 Category2]@
    | Intersect -- ^ Written @[Category1 +Category2]@
    | Subtract  -- ^ Written @[Category1 -Category2]@
    deriving (Show, Eq, Ord, Generic, NFData)

-- | A single element of a category: a sequence of 'Lexemes'. (Single
-- 'Grapheme's receive some special treatment, e.g. they can be
-- written without surrounding braces in Brassica syntax.)
type CategoryElement category a = [Lexeme category a]

-- | The specification of a category in Brassica sound-change
-- syntax. Usually this will be as a 'CategorySpec': a list of
-- 'CategoryElement's, each of which modifies the previous definition
-- using the given 'CategoryModification' method.
--
-- In some positions (e.g. after a 'Backreference') a category must be
-- provided, but that category can be predefined, to be inlined during
-- expansion. In such positions, the given category name is stored as
-- a 'MustInline' category. (In other positions predefined categories
-- are indistinguishable from normal v'Grapheme's, and represented as
-- such.)
data CategorySpec a
    = CategorySpec [(CategoryModification, CategoryElement CategorySpec a)]
    | MustInline String  -- ^ A single grapheme assumed to have been specified earlier as a category
    deriving (Show, Eq, Ord, Generic, NFData)

-- | The specification of a suprasegmental feature in Brassica
-- sound-change syntax.
--
-- Deprecated since 1.0.0.
data FeatureSpec = FeatureSpec
    { featureBaseName :: Maybe String
    , featureBaseValues :: CategorySpec 'AnyPart
    , featureDerived :: [(String, CategorySpec 'AnyPart)]
    }
    deriving (Show, Eq, Ord, Generic, NFData)

-- | A single definition within a category definition block.
data CategoryDefinition
    = DefineCategory String (CategorySpec 'AnyPart)
    -- ^ Defines a category with the given name and value
    | DefineFeature FeatureSpec
    -- ^ Defines a feature as a set of categories
    | DefineAuto String
    -- ^ Defines a category as autosegmental
    deriving (Show, Eq, Ord, Generic, NFData)

-- | A directive used in Brassica sound-change syntax: anything which
-- is not actually a sound change (TODO find better name)
data Directive
    = Categories  -- ^ Category definition block
        Bool  -- ^ Whether category was introduced with @new@
        Bool  -- ^ Whether category was introduced with @noreplace@
        [CategoryDefinition]
    | ExtraGraphemes [String]
      -- ^ Extra graphemes declaration: @extra …@
    deriving (Show, Eq, Ord, Generic, NFData)

-- | A list of graphemes, replacing v'Categories' in expanded sound
-- changes. These are used in tokenisation to determine which
-- multigraphs are used, and in rule application to filter unwanted
-- graphemes. The first 'Bool' indicates whether filtration should
-- occur for any particular categories block.
type GraphemeList = (Bool, [Grapheme])
