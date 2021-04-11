{-# LANGUAGE DeriveFunctor #-}

module SoundChange where

import SoundChange.Apply
import SoundChange.Category
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

tokeniseAnd :: ([Rule] -> [Grapheme] -> a) -> Categories Grapheme -> [Rule] -> String -> [Component a]
tokeniseAnd action cats rs ws =
    let ts = tokeniseWords (values cats) ws
    in (fmap.fmap) (action rs) ts

data LogItem r = RuleApplied
    { rule :: r
    , input :: [Grapheme]
    , output :: [Grapheme]
    } deriving (Show, Functor)

applyRuleWithLog :: Rule -> [Grapheme] -> Maybe (LogItem Rule)
applyRuleWithLog r w =
    let w' = applyStr r w
    in if w' == w then Nothing else Just (RuleApplied r w w')

applyRulesWithLog :: [Rule] -> [Grapheme] -> [LogItem Rule]
applyRulesWithLog [] _ = []
applyRulesWithLog (r:rs) w =
    case applyRuleWithLog r w of
        Nothing -> applyRulesWithLog rs w
        Just l@RuleApplied{output=w'} -> l : applyRulesWithLog rs w'

applyRules :: [Rule] -> [Grapheme] -> [Grapheme]
applyRules rs w = case applyRulesWithLog rs w of
    [] -> w
    logs -> output $ last logs

applyRulesWithChanges :: [Rule] -> [Grapheme] -> ([Grapheme], Bool)
applyRulesWithChanges rs w = case applyRulesWithLog rs w of
    [] -> (w, False)
    logs -> (output $ last logs, hasChanged logs)
  where
    hasChanged = any $ highlightChanges . flags . rule
