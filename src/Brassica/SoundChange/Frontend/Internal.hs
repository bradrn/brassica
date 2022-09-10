{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TupleSections   #-}

module Brassica.SoundChange.Frontend.Internal where

import Data.Bifunctor (second)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)
import Text.Megaparsec (ParseErrorBundle)

import Brassica.MDF (MDF, parseMDFWithTokenisation, componentiseMDF, componentiseMDFWordsOnly, duplicateEtymologies)
import Brassica.SoundChange.Apply
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types

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
    -> Either (ParseErrorBundle String Void) (ParseOutput PWord)
tokeniseAccordingToInputFormat Raw _ cs =
    fmap ParsedRaw . withFirstCategoriesDecl tokeniseWords cs
tokeniseAccordingToInputFormat MDF Normal cs =
    fmap ParsedMDF . withFirstCategoriesDecl parseMDFWithTokenisation cs
tokeniseAccordingToInputFormat MDF AddEtymons cs =
    fmap ParsedMDF
    . second (duplicateEtymologies $ ('*':) . detokeniseWords)
    . withFirstCategoriesDecl parseMDFWithTokenisation cs

-- | Top-level dispatcher for an interactive frontend: given a textual
-- wordlist and a list of sound changes, returns the result of running
-- the changes in the specified mode.
parseTokeniseAndApplyRules
    :: SoundChanges -- ^ changes
    -> String       -- ^ words
    -> InputLexiconFormat
    -> TokenisationMode
    -> ApplicationMode
    -> Maybe [Component PWord]  -- ^ previous results
    -> ApplicationOutput PWord Statement
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
