{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

module SoundChange where

import SoundChange.Apply
import SoundChange.Parse
import SoundChange.Types

-- tokeniseAndApplyRulesWithLog :: Categories Grapheme -> [Rule] -> String -> [Component [LogItem Rule]]
-- tokeniseAndApplyRulesWithLog cats rs ws =
--     let ts = (fmap.fmap.fmap) Right $ tokeniseWords (values cats) ws
--     in (fmap.fmap) (applyRulesWithLog rs) ts
    
-- tokeniseAndApplyRules :: Categories Grapheme -> [Rule] -> String -> [Component [WordPart]]
-- tokeniseAndApplyRules cats rs ws =
--     let ts = (fmap.fmap.fmap) Right $ tokeniseWords (values cats) ws
--     in (fmap.fmap) (applyRules rs) ts

tokeniseAnd :: (SoundChanges -> [Grapheme] -> a) -> SoundChanges -> String -> [Component a]
tokeniseAnd applyFn sts ws =
    let gs = findFirstCategoriesDecl sts
        ts = tokeniseWords gs ws
    in (fmap.fmap) (applyFn sts) ts
  where
    findFirstCategoriesDecl (CategoriesDeclS (CategoriesDecl gs):_) = gs
    findFirstCategoriesDecl (_:ss) = findFirstCategoriesDecl ss
    findFirstCategoriesDecl [] = []

data LogItem r = ActionApplied
    { action :: r
    , input :: [Grapheme]
    , output :: [Grapheme]
    } deriving (Show, Functor)

applyStatementWithLog :: Statement -> [Grapheme] -> Maybe (LogItem Statement)
applyStatementWithLog st w =
    let w' = applyStatementStr st w
    in if w' == w then Nothing else Just (ActionApplied st w w')

applyChangesWithLog :: SoundChanges -> [Grapheme] -> [LogItem Statement]
applyChangesWithLog [] _ = []
applyChangesWithLog (st:sts) w =
    case applyStatementWithLog st w of
        Nothing -> applyChangesWithLog sts w
        Just l@ActionApplied{output=w'} -> l : applyChangesWithLog sts w'

applyChanges :: SoundChanges -> [Grapheme] -> [Grapheme]
applyChanges sts w = case applyChangesWithLog sts w of
    [] -> w
    logs -> output $ last logs

applyChangesWithChanges :: SoundChanges -> [Grapheme] -> ([Grapheme], Bool)
applyChangesWithChanges sts w = case applyChangesWithLog sts w of
    [] -> (w, False)
    logs -> (output $ last logs, hasChanged logs)
  where
    hasChanged = any $ \case
        ActionApplied{action=RuleS rule} -> highlightChanges $ flags rule
        ActionApplied{action=CategoriesDeclS _} -> True
