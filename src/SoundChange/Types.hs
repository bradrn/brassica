{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
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

module SoundChange.Types where

import Data.Kind (Constraint)
import GHC.TypeLits
import SoundChange.Category (Categories)

-- | The constraint @OneOf a x y@ is satisfied if @a ~ x@ or @a ~ y@.
--
-- (Note: the strange @() ~ Bool@ constraint is just a simple
-- unsatisfyable constraint, so as to not give ‘non-exhaustive pattern
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

-- | The type of graphemes (or more accurately multigraphs): a
-- grapheme is a sequence of characters. A word (or a subsequence of
-- one) can be considered to be a @[Grapheme]@.
type Grapheme = [Char]

-- | The part of a 'Rule' in which a 'Lexeme' may occur: either the
-- target, the replacement or the environment.
data LexemeType = Target | Replacement | Env

data SLexemeType :: LexemeType -> * where
    STarget      :: SLexemeType 'Target
    SReplacement :: SLexemeType 'Replacement
    SEnv         :: SLexemeType 'Env

class SingLT lt where singLT :: SLexemeType lt
instance SingLT 'Target      where singLT = STarget
instance SingLT 'Replacement where singLT = SReplacement
instance SingLT 'Env         where singLT = SEnv

-- | A 'Lexeme' is the smallest component of a sound change,
-- specifying either a match or a replacement. The phantom type
-- variable, of kind 'LexemeType', specifies the part(s) of the rule
-- in which each type of 'Lexeme' may occur.
data Lexeme (a :: LexemeType) where
    Grapheme :: Grapheme -> Lexeme a
    Category :: [CategoryElement a] -> Lexeme a
    Boundary :: Lexeme 'Env
    Optional :: [Lexeme a] -> Lexeme a
    Metathesis :: Lexeme 'Replacement
    Geminate :: Lexeme a
    Wildcard :: OneOf a 'Target 'Env => Lexeme a -> Lexeme a
    Kleene   :: OneOf a 'Target 'Env => Lexeme a -> Lexeme a
    Discard  :: Lexeme 'Replacement

deriving instance Show (Lexeme a)

-- | The elements allowed in a 'Category'.
data CategoryElement (a :: LexemeType) where
    GraphemeEl :: Grapheme -> CategoryElement a
    BoundaryEl :: CategoryElement 'Env

deriving instance Show (CategoryElement a)
deriving instance Eq (CategoryElement a)
deriving instance Ord (CategoryElement a)

-- | An 'Environment' is a tuple of @(before, after)@ components,
-- corresponding to a ‘/ before _ after’ component of a sound change.
--
-- Note that an empty environment is just @([], [])@.
type Environment = ([Lexeme 'Env], [Lexeme 'Env])

-- | Specifies application direction of rule — either left-to-right or right-to-left.
data Direction = LTR | RTL
    deriving (Eq, Show)

-- | Flags which can be enabled on a 'Rule'
data Flags = Flags
  { highlightChanges :: Bool
  , applyDirection   :: Direction
  , applyOnceOnly    :: Bool
  } deriving (Show)

defFlags :: Flags
defFlags = Flags True LTR False

-- | A sound change rule: ‘-flags target → replacement \/ environment \/ exception’.
data Rule = Rule
  { target      :: [Lexeme 'Target]
  , replacement :: [Lexeme 'Replacement]
  , environment :: Environment
  , exception   :: Maybe Environment
  , flags       :: Flags
  , plaintext   :: String
  } deriving (Show)

newtype CategoriesDecl = CategoriesDecl { graphemes :: [Grapheme] }
  deriving (Show)

data Statement = RuleS Rule | CategoriesDeclS CategoriesDecl
    deriving (Show)

type SoundChanges = [Statement]
