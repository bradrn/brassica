{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

module Brassica.SoundChange where

import Data.Void (Void)

import Text.Megaparsec (ParseErrorBundle)

import Brassica.SoundChange.Apply
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types

-- | Tokenises the input 'String' into a @[Grapheme]@, then applies
-- the given 'SoundChanges' to the input using a user-specified
-- application function.
tokeniseAnd :: (SoundChanges -> [Grapheme] -> a) -> SoundChanges -> String -> Either (ParseErrorBundle String Void) [Component a]
tokeniseAnd applyFn sts ws =
    let gs = findFirstCategoriesDecl sts
        ts = tokeniseWords gs ws
    in (fmap.fmap.fmap) (applyFn sts) ts
  where
    findFirstCategoriesDecl (CategoriesDeclS (CategoriesDecl gs):_) = gs
    findFirstCategoriesDecl (_:ss) = findFirstCategoriesDecl ss
    findFirstCategoriesDecl [] = []

-- | A log item representing a single application of an action. (In
-- practise this will usually be a 'Statement'.) Specifies the action
-- which was applied, as well as the ‘before’ and ‘after’ states.
data LogItem r = ActionApplied
    { action :: r
    , input :: [Grapheme]
    , output :: [Grapheme]
    } deriving (Show, Functor)

-- | Apply a single 'Statement' to a word. Returns a 'LogItem' only if
-- the statement altered the input.
applyStatementWithLog :: Statement -> [Grapheme] -> Maybe (LogItem Statement)
applyStatementWithLog st w =
    let w' = applyStatementStr st w
    in if w' == w then Nothing else Just (ActionApplied st w w')

-- | Apply 'SoundChanges' to a word. Returns a 'LogItem' for each
-- 'Statement' which altered the input.
applyChangesWithLog :: SoundChanges -> [Grapheme] -> [LogItem Statement]
applyChangesWithLog [] _ = []
applyChangesWithLog (st:sts) w =
    case applyStatementWithLog st w of
        Nothing -> applyChangesWithLog sts w
        Just l@ActionApplied{output=w'} -> l : applyChangesWithLog sts w'

-- | Apply 'SoundChanges' to a word returning the final result without
-- any log.
applyChanges :: SoundChanges -> [Grapheme] -> [Grapheme]
applyChanges sts w = case applyChangesWithLog sts w of
    [] -> w
    logs -> output $ last logs

-- | Apply 'SoundChanges' to a word returning the final result, as
-- well as a boolean value indicating whether the word should be
-- highlighted in a UI due to changes from its initial value. (Note
-- that this accounts for 'highlightChanges' values.)
applyChangesWithChanges :: SoundChanges -> [Grapheme] -> ([Grapheme], Bool)
applyChangesWithChanges sts w = case applyChangesWithLog sts w of
    [] -> (w, False)
    logs -> (output $ last logs, hasChanged logs)
  where
    hasChanged = any $ \case
        ActionApplied{action=RuleS rule} -> highlightChanges $ flags rule
        ActionApplied{action=CategoriesDeclS _} -> True
