{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Brassica.SoundChange where

import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Text.Megaparsec (ParseErrorBundle)

import Brassica.MDF (MDF, parseMDFWithTokenisation, componentiseMDF, componentiseMDFWordsOnly, duplicateEtymologies)
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

-- | Apply a single 'Statement' to a word. Returns a 'LogItem' for
-- each possible result, or @[]@ if the rule does not apply and the
-- input is returned unmodified.
applyStatementWithLog :: Statement -> [Grapheme] -> [LogItem Statement]
applyStatementWithLog st w = case applyStatementStr st w of
    [w'] -> if w' == w then [] else [ActionApplied st w w']
    r -> ActionApplied st w <$> r

-- | Apply 'SoundChanges' to a word. For each possible result, returns
-- a 'LogItem' for each 'Statement' which altered the input.
applyChangesWithLog :: SoundChanges -> [Grapheme] -> [[LogItem Statement]]
applyChangesWithLog [] _ = [[]]
applyChangesWithLog (st:sts) w =
    case applyStatementWithLog st w of
        [] -> applyChangesWithLog sts w
        items -> items >>= \l@ActionApplied{output=w'} ->
            (l :) <$> applyChangesWithLog sts w'

-- | Apply 'SoundChanges' to a word returning the final results
-- without any logs.
applyChanges :: SoundChanges -> [Grapheme] -> [[Grapheme]]
applyChanges sts w =
    lastOutput <$> applyChangesWithLog sts w
  where
    lastOutput [] = w
    lastOutput ls = output $ last ls

-- | Apply 'SoundChanges' to a word returning the final results, as
-- well as a boolean value indicating whether the word should be
-- highlighted in a UI due to changes from its initial value. (Note
-- that this accounts for 'highlightChanges' values.)
applyChangesWithChanges :: SoundChanges -> [Grapheme] -> [([Grapheme], Bool)]
applyChangesWithChanges sts w = applyChangesWithLog sts w <&> \case
    [] -> (w, False)
    logs -> (output $ last logs, hasChanged logs)
  where
    hasChanged = any $ \case
        ActionApplied{action=RuleS rule} -> highlightChanges $ flags rule
        ActionApplied{action=CategoriesDeclS _} -> True

-- | Rule application mode of the SCA.
data ApplicationMode
    = ApplyRules HighlightMode MDFOutputMode
    | ReportRulesApplied
    deriving (Show, Eq)

data HighlightMode
    = NoHighlight
    | DifferentToLastRun
    | DifferentToInput
    deriving (Show, Eq)
instance Enum HighlightMode where
    -- used for conversion to and from C, so want control over values
    fromEnum NoHighlight = 0
    fromEnum DifferentToLastRun = 1
    fromEnum DifferentToInput = 2

    toEnum 0 = NoHighlight
    toEnum 1 = DifferentToLastRun
    toEnum 2 = DifferentToInput
    toEnum _ = undefined

data MDFOutputMode = MDFOutput | WordsOnlyOutput
    deriving (Show, Eq)
instance Enum MDFOutputMode where
    -- used for conversion to and from C, so want control over values
    fromEnum MDFOutput = 0
    fromEnum WordsOnlyOutput = 1

    toEnum 0 = MDFOutput
    toEnum 1 = WordsOnlyOutput
    toEnum _ = undefined

data TokenisationMode = Normal | AddEtymons
    deriving (Show, Eq)
instance Enum TokenisationMode where
    -- used for conversion to and from C, so want control over values
    fromEnum Normal = 0
    fromEnum AddEtymons = 1

    toEnum 0 = Normal
    toEnum 1 = AddEtymons
    toEnum _ = undefined

-- | Output of a single application of rules to a wordlist: either a
-- list of possibly highlighted words, an applied rules table, or a
-- parse error.
data ApplicationOutput a r
    = HighlightedWords [Component (a, Bool)]
    | AppliedRulesTable [AppliedRulesTableItem r]
    | ParseError (ParseErrorBundle String Void)
    deriving (Show, Generic, NFData)

-- | Kind of input: either a raw wordlist, or an MDF file.
data InputLexiconFormat = Raw | MDF
    deriving (Show, Eq)
instance Enum InputLexiconFormat where
    -- used for conversion to and from C, so want control over values
    fromEnum Raw = 0
    fromEnum MDF = 1

    toEnum 0 = Raw
    toEnum 1 = MDF
    toEnum _ = undefined

data ParseOutput a = ParsedRaw [Component a] | ParsedMDF (MDF [Component a])
    deriving (Show, Functor)

componentise :: MDFOutputMode -> ParseOutput a -> [Component a]
componentise _               (ParsedRaw cs) = cs
componentise MDFOutput       (ParsedMDF mdf) = componentiseMDF mdf
componentise WordsOnlyOutput (ParsedMDF mdf) = componentiseMDFWordsOnly mdf

tokeniseAccordingToInputFormat
    :: InputLexiconFormat
    -> TokenisationMode
    -> SoundChanges
    -> String
    -> Either (ParseErrorBundle String Void) (ParseOutput [Grapheme])
tokeniseAccordingToInputFormat Raw _ cs =
    fmap ParsedRaw . withFirstCategoriesDecl tokeniseWords cs
tokeniseAccordingToInputFormat MDF Normal cs =
    fmap ParsedMDF . withFirstCategoriesDecl parseMDFWithTokenisation cs
tokeniseAccordingToInputFormat MDF AddEtymons cs =
    fmap ParsedMDF
    . second (duplicateEtymologies $ ('*':) . detokeniseWords)
    . withFirstCategoriesDecl parseMDFWithTokenisation cs

-- | Utility function: insert 'Whitespace' (hard-coded as a single
-- space) between multiple results.
splitMultipleResults :: Component [a] -> [Component a]
splitMultipleResults (Word as) = intersperse (Whitespace " ") $ Word <$> as
splitMultipleResults (Whitespace w) = [Whitespace w]
splitMultipleResults (Gloss g) = [Gloss g]

-- | Top-level dispatcher for an interactive frontend: given a textual
-- wordlist and a list of sound changes, returns the result of running
-- the changes in the specified mode.
parseTokeniseAndApplyRules
    :: SoundChanges -- ^ changes
    -> String       -- ^ words
    -> InputLexiconFormat
    -> TokenisationMode
    -> ApplicationMode
    -> Maybe [Component [Grapheme]]  -- ^ previous results
    -> ApplicationOutput [Grapheme] Statement
parseTokeniseAndApplyRules statements ws intype tmode mode prev =
    case tokeniseAccordingToInputFormat intype tmode statements ws of
        Left e -> ParseError e
        Right toks -> case mode of
            ReportRulesApplied ->
                AppliedRulesTable $ mapMaybe toTableItem $ concat $
                    getWords $ componentise WordsOnlyOutput $
                        applyChangesWithLog statements <$> toks
            ApplyRules DifferentToLastRun mdfout ->
                let result = concatMap splitMultipleResults $
                      componentise mdfout $ applyChanges statements <$> toks
                in HighlightedWords $
                    zipWithComponents result (fromMaybe [] prev) [] $ \thisWord prevWord ->
                        (thisWord, thisWord /= prevWord)
            ApplyRules DifferentToInput mdfout ->
                HighlightedWords $ concatMap splitMultipleResults $
                    componentise mdfout $ applyChangesWithChanges statements <$> toks
            ApplyRules NoHighlight mdfout ->
                HighlightedWords $ (fmap.fmap) (,False) $ concatMap splitMultipleResults $
                    componentise mdfout $ applyChanges statements <$> toks
