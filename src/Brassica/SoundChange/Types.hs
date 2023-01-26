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
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Brassica.SoundChange.Types
       (
       -- * Words and graphemes
         Grapheme
       , PWord
       -- * Lexemes
       , Lexeme(..)
       , CategoryElement(..)
       , LexemeType(..)
       -- * Rules
       , Rule(..)
       , Environment
       , Direction(..)
       , Flags(..)
       , defFlags
       -- * Categories and statements
       , CategoriesDecl(..)
       , Statement(..)
       , plaintext'
       , SoundChanges
       -- * Utility
       , OneOf
       ) where

import Control.DeepSeq (NFData(..))
import Data.Kind (Constraint)
import GHC.Generics (Generic)
import GHC.TypeLits

-- | The constraint @OneOf a x y@ is satisfied if @a ~ x@ or @a ~ y@.
--
-- (Note: the strange @() ~ Bool@ constraint is just a simple
-- unsatisfiable constraint, so as to not give ‘non-exhaustive pattern
-- match’ errors everywhere.)
type family OneOf a x y :: Constraint where
    OneOf a a y = ()
    OneOf a x a = ()
    OneOf a b c =
        ( () ~ Bool
        , TypeError ('Text "Couldn't match type "
                     ':<>: 'ShowType a
                     ':<>: 'Text " with "
                     ':<>: 'ShowType b
                     ':<>: 'Text " or "
                     ':<>: 'ShowType c))

-- | The type of graphemes, or more accurately multigraphs: for
-- instance, @"a", "ch", "c̓" :: t'Grapheme'@.
type Grapheme = [Char]

-- | A word (or a subsequence of one) can be viewed as a list of
-- @Grapheme@s: e.g. Portuguese "filha" becomes
-- @["f", "i", "lh", "a"] :: 'PWord'@.
--
-- (The name 'PWord' is from ‘phonological word’, these being what a
-- SCA typically manipulates; this name was chosen to avoid a clash
-- with @Prelude.'Prelude.Word'@.)
type PWord = [Grapheme]

-- | The part of a 'Rule' in which a 'Lexeme' may occur: either the
-- target, the replacement or the environment.
data LexemeType = Target | Replacement | Env

-- | A 'Lexeme' is the smallest part of a sound change. Both matches
-- and replacements are made up of 'Lexeme's: the phantom type
-- variable specifies where each different variety of 'Lexeme' may
-- occur.
data Lexeme (a :: LexemeType) where
    -- | In Brassica sound-change syntax, one or more letters without intervening whitespace
    Grapheme :: Grapheme -> Lexeme a
    -- | In Brassica sound-change syntax, delimited by square brackets
    Category :: [CategoryElement a] -> Lexeme a
    -- | In Brassica sound-change syntax, specified as @#@
    Boundary :: Lexeme 'Env
    -- | In Brassica sound-change syntax, delimited by parentheses
    Optional :: [Lexeme a] -> Lexeme a
    -- | In Brassica sound-change syntax, specified as @\@
    Metathesis :: Lexeme 'Replacement
    -- | In Brassica sound-change syntax, specified as @>@
    Geminate :: Lexeme a
    -- | In Brassica sound-change syntax, specified as @^@ before another 'Lexeme'
    Wildcard :: OneOf a 'Target 'Env => Lexeme a -> Lexeme a
    -- | In Brassica sound-change syntax, specified as @*@ after another 'Lexeme'
    Kleene   :: OneOf a 'Target 'Env => Lexeme a -> Lexeme a
    -- | In Brassica sound-change syntax, specified as @~@
    Discard  :: Lexeme 'Replacement
    -- | In Brassica sound-change syntax, specified as \@i before a category
    Backreference :: OneOf a 'Target 'Replacement => Int -> [CategoryElement a] -> Lexeme a
    -- | In Brassica sound-change syntax, specified as \@? before a category
    Multiple :: [CategoryElement 'Replacement] -> Lexeme 'Replacement

deriving instance Show (Lexeme a)

instance NFData (Lexeme a) where
    rnf (Grapheme g) = rnf g
    rnf (Category cs) = rnf cs
    rnf Boundary = ()
    rnf (Optional ls) = rnf ls
    rnf Metathesis = ()
    rnf Geminate = ()
    rnf (Wildcard l) = rnf l
    rnf (Kleene l) = rnf l
    rnf Discard = ()
    rnf (Backreference i l) = seq i $ rnf l
    rnf (Multiple l) = rnf l

-- | The elements allowed in a 'Category': currently, only
-- t'Grapheme's and word boundaries.
data CategoryElement (a :: LexemeType) where
    GraphemeEl :: Grapheme -> CategoryElement a
    BoundaryEl :: CategoryElement 'Env

deriving instance Show (CategoryElement a)
deriving instance Eq (CategoryElement a)
deriving instance Ord (CategoryElement a)

instance NFData (CategoryElement a) where
    rnf (GraphemeEl a) = rnf a
    rnf BoundaryEl = ()

-- | An 'Environment' is a tuple of @(before, after)@ components,
-- corresponding to a ‘/ before _ after’ component of a sound change.
--
-- Note that an empty environment is just @([], [])@.
type Environment = ([Lexeme 'Env], [Lexeme 'Env])

-- | Specifies application direction of rule — either left-to-right or right-to-left.
data Direction = LTR | RTL
    deriving (Eq, Show, Generic, NFData)

-- | Flags which can be enabled, disabled or altered on a 'Rule' to
-- change how it is applied.
data Flags = Flags
  { highlightChanges :: Bool
  , applyDirection   :: Direction
  , applyOnceOnly    :: Bool
  , sporadic         :: Bool
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
    , sporadic = False
    }

-- | A single sound change rule: in Brassica sound-change syntax with all elements specified,
-- @-flags target / replacement \/ environment \/ exception@.
-- (And usually the 'plaintext' of the rule will contain a 'String' resembling that pattern.)
data Rule = Rule
  { target      :: [Lexeme 'Target]
  , replacement :: [Lexeme 'Replacement]
  , environment :: Environment
  , exception   :: Maybe Environment
  , flags       :: Flags
  , plaintext   :: String
  } deriving (Show, Generic, NFData)

-- | Corresponds to a category declaration in a set of sound
-- changes. Category declarations are mostly desugared away by the
-- parser, but for rule application we still need to be able to filter
-- out all unknown t'Grapheme's; thus, a 'CategoriesDecl' lists the
-- t'Grapheme's which are available at a given point.
newtype CategoriesDecl = CategoriesDecl { graphemes :: [Grapheme] }
  deriving (Show, Generic, NFData)

-- | A 'Statement' can be either a single sound change rule, or a
-- category declaration.
data Statement = RuleS Rule | CategoriesDeclS CategoriesDecl
    deriving (Show, Generic, NFData)

-- | A simple wrapper around 'plaintext' for 'Statement's. Returns
-- @"categories … end"@ for all 'CategoriesDecl' inputs.
plaintext' :: Statement -> String
plaintext' (RuleS r) = plaintext r
plaintext' (CategoriesDeclS _) = "categories … end"

-- | A set of 'SoundChanges' is simply a list of 'Statement's.
type SoundChanges = [Statement]
