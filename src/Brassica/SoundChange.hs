{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Brassica.SoundChange where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Text.Megaparsec (ParseErrorBundle)

import Brassica.SoundChange.Apply
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types

-- | A log item representing a single application of an action. (In
-- practise this will usually be a 'Statement'.) Specifies the action
-- which was applied, as well as the ‘before’ and ‘after’ states.
data LogItem r = ActionApplied
    { action :: r
    , input :: [Grapheme]
    , output :: [Grapheme]
    } deriving (Show, Functor, Generic, NFData)

-- | A single component of an ‘applied rules’ table, which collates
-- action applications by the word they are applied to.
data AppliedRulesTableItem r = AppliedRulesTableItem
    { initialWord :: [Grapheme]
    , derivations :: [([Grapheme], r)]
    } deriving (Show, Functor, Generic, NFData)

toTableItem :: [LogItem r] -> Maybe (AppliedRulesTableItem r)
toTableItem [] = Nothing
toTableItem ls@(l : _) = Just $ AppliedRulesTableItem
    { initialWord = input l
    , derivations = (\ActionApplied{..} -> (output, action)) <$> ls
    }

-- | Render a single 'AppliedRulesTableItem' to HTML table rows.
tableItemToHtmlRows :: (r -> String) -> AppliedRulesTableItem r -> String
tableItemToHtmlRows render item = go (concat $ initialWord item) (derivations item)
  where
    go _ [] = ""
    go cell1 ((output, action) : ds) =
        ("<tr><td>" ++ cell1 ++ "</td><td>&rarr;</td>\
        \<td>" ++ concat output ++ "</td><td>(" ++ render action ++ ")</td></tr>")
        ++ go "" ds

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

-- | Output mode of the SCA.
data OutputMode
    = NoHighlight
    | DifferentToLastRun
    | DifferentToInput
    | ReportRulesApplied
    deriving (Show, Eq)
instance Enum OutputMode where
    -- used for conversion to and from C, so want control over values
    fromEnum NoHighlight = 0
    fromEnum DifferentToLastRun = 1
    fromEnum DifferentToInput = 2
    fromEnum ReportRulesApplied = 3

    toEnum 0 = NoHighlight
    toEnum 1 = DifferentToLastRun
    toEnum 2 = DifferentToInput
    toEnum 3 = ReportRulesApplied
    toEnum _ = undefined

-- | Output of a single application of rules to a wordlist: either a
-- list of possibly highlighted words, an applied rules table, or a
-- parse error.
data ApplicationOutput a r
    = HighlightedWords [Component (a, Bool)]
    | AppliedRulesTable [AppliedRulesTableItem r]
    | ParseError (ParseErrorBundle String Void)
    deriving (Show, Generic, NFData)

-- | Top-level dispatcher for an interactive frontend: given a textual
-- wordlist and a list of sound changes, returns the result of running
-- the changes in the specified mode.
parseTokeniseAndApplyRules
    :: SoundChanges -- ^ changes
    -> String       -- ^ words
    -> OutputMode
    -> Maybe [Component [Grapheme]]  -- ^ previous results
    -> ApplicationOutput [Grapheme] Statement
parseTokeniseAndApplyRules statements ws mode prev =
    case withFirstCategoriesDecl tokeniseWords statements ws of
        Left e -> ParseError e
        Right toks -> case mode of
            ReportRulesApplied ->
                AppliedRulesTable $ mapMaybe toTableItem $ getWords $
                    applyChangesWithLog statements <<$>> toks
            DifferentToLastRun ->
                let result = applyChanges statements <<$>> toks
                in HighlightedWords $
                    zipWithComponents result (fromMaybe [] prev) [] $ \thisWord prevWord ->
                        (thisWord, thisWord /= prevWord)
            DifferentToInput ->
                HighlightedWords $ applyChangesWithChanges statements <<$>> toks
            NoHighlight ->
                HighlightedWords $ ((,False) . applyChanges statements) <<$>> toks
  where
    (<<$>>) = fmap . fmap
