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
import Brassica.SoundChange.Apply.Internal (applyChangesWithLog, toTableItem)
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
  where
    -- Zips two tokenised input strings. Compared to normal 'zipWith'
    -- this has two special properties:
    --
    --   * It only zips v'Word's. Any non-v'Word's in the first argument
    --     will be passed unaltered to the output; any in the second
    --     argument will be ignored.
    --
    --   * The returned list will have the same number of elements as does
    --     the first argument. If a v'Word' in the first argument has no
    --     corresponding v'Word' in the second, the zipping function is
    --     called using the default @b@ value given as the third argument.
    --     Such a v'Word' in the second argument will simply be ignored.
    --
    -- Note the persistent assymetry in the definition: each 'Component'
    -- in the first argument will be reflected in the output, but each in
    -- the second argument may be ignored.
    zipWithComponents :: [Component a] -> [Component b] -> b -> (a -> b -> c) -> [Component c]
    zipWithComponents []             _            _  _ = []
    zipWithComponents as            []            bd f = (fmap.fmap) (`f` bd) as
    zipWithComponents (Word a:as)   (Word b:bs)   bd f = Word (f a b) : zipWithComponents as bs bd f
    zipWithComponents as@(Word _:_) (_:bs)        bd f = zipWithComponents as bs bd f
    zipWithComponents (a:as)        bs@(Word _:_) bd f = unsafeCastComponent a : zipWithComponents as bs bd f
    zipWithComponents (a:as)        (_:bs)        bd f = unsafeCastComponent a : zipWithComponents as bs bd f

    unsafeCastComponent :: Component a -> Component b
    unsafeCastComponent (Word _) = error "unsafeCastComponent: attempted to cast a word!"
    unsafeCastComponent (Whitespace s) = Whitespace s
    unsafeCastComponent (Gloss s) = Gloss s
