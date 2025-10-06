{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}

-- |
-- Module      : Brassica.SoundChange.Frontend.Internal
-- Copyright   : See LICENSE file
-- License     : BSD3
-- Maintainer  : Brad Neimann
--
-- __Warning:__ This module is __internal__, and does __not__ follow
-- the Package Versioning Policy. It may be useful for extending
-- Brassica, but be prepared to track development closely if you import
-- this module.
--
-- This module exists primarily as an internal common interface for
-- Brassica’s two ‘official’ GUI frontends (desktop and web). If you
-- wish to make your own frontend to Brassica, it is probably easier
-- to write it yourself rather than trying to use this.
module Brassica.SoundChange.Frontend.Internal where

import Control.Monad ((<=<))
import Data.Containers.ListUtils (nubOrd)
import Data.List (transpose, intersperse, intersect)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Void (Void)
import GHC.Generics (Generic)
import Myers.Diff (getDiff, PolyDiff(..))

import Control.DeepSeq (NFData)
import Text.Megaparsec (ParseErrorBundle)

import Brassica.SFM.MDF
import Brassica.SFM.SFM
import Brassica.SoundChange.Apply hiding (HighlightMode)
import qualified Brassica.SoundChange.Apply as A
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types

-- | Rule application mode of the SCA.
data ApplicationMode
    = ApplyRules HighlightMode OutputMode String
    -- ^ Apply sound changes as normal, with the given modes and
    -- separator
    | ReportRules ReportMode
    -- ^ Apply reporting rules (as HTML)
    deriving (Show, Eq)

data ReportMode
    = ReportApplied
    -- ^ Report the rules which were applied
    | ReportNotApplied
    -- ^ Report the rules which were not applied
    deriving (Show, Eq)
instance Enum ReportMode where
    -- used for conversion to and from C, so want control over values
    -- special-case for this type: reserve 0 for no reporting
    fromEnum ReportApplied = 1
    fromEnum ReportNotApplied = 2

    toEnum 1 = ReportApplied
    toEnum 2 = ReportNotApplied
    toEnum _ = undefined

-- | Get the 'OutputMode' if one is set, otherwise default to
-- 'WordsOnlyOutput'.
getOutputMode :: ApplicationMode -> OutputMode
getOutputMode (ApplyRules _ o _) = o
getOutputMode (ReportRules _) = WordsOnlyOutput

-- | Mode for highlighting output words
data HighlightMode
    = NoHighlight
    | DifferentToLastRun
    | DifferentToInput A.HighlightMode
    -- ^ NB. now labeled ‘any rule applied’ in GUI
    deriving (Show, Eq)
instance Enum HighlightMode where
    -- used for conversion to and from C, so want control over values
    fromEnum NoHighlight = 0
    fromEnum DifferentToLastRun = 1
    fromEnum (DifferentToInput AllChanged) = 2
    fromEnum (DifferentToInput SpecificRule) = 3

    toEnum 0 = NoHighlight
    toEnum 1 = DifferentToLastRun
    toEnum 2 = DifferentToInput AllChanged
    toEnum 3 = DifferentToInput SpecificRule
    toEnum _ = undefined

-- | Mode for reporting output words (and sometimes intermediate and
-- input words too)
data OutputMode
    = MDFOutput
    | WordsOnlyOutput
    | MDFOutputWithEtymons
    | WordsWithProtoOutput
    | WordsWithProtoOutputPreserve
    deriving (Show, Eq)
instance Enum OutputMode where
    -- used for conversion to and from C, so want control over values
    fromEnum MDFOutput = 0
    fromEnum WordsOnlyOutput = 1
    fromEnum MDFOutputWithEtymons = 2
    fromEnum WordsWithProtoOutput = 3
    fromEnum WordsWithProtoOutputPreserve = 4

    toEnum 0 = MDFOutput
    toEnum 1 = WordsOnlyOutput
    toEnum 2 = MDFOutputWithEtymons
    toEnum 3 = WordsWithProtoOutput
    toEnum 4 = WordsWithProtoOutputPreserve
    toEnum _ = undefined

-- | Output of a single application of rules to a wordlist: either a
-- list of possibly highlighted words, an applied rules table, or a
-- parse error.
data ApplicationOutput a r
    = HighlightedWords [Component (a, Bool)]
    | AppliedRulesTable [Log r]
    | NotAppliedRulesList [r]
    | ParseError (ParseErrorBundle String Void)
    deriving (Show, Generic, NFData)

-- | For MDF input, the hierarchy used
data MDFHierarchy = Standard | Alternate
    deriving (Show, Eq)

-- | Kind of input: either a raw wordlist, or an MDF file.
data InputLexiconFormat = Raw | MDF MDFHierarchy
    deriving (Show, Eq)
instance Enum InputLexiconFormat where
    -- used for conversion to and from C, so want control over values
    fromEnum Raw = 0
    fromEnum (MDF Standard) = 1
    fromEnum (MDF Alternate) = 2

    toEnum 0 = Raw
    toEnum 1 = MDF Standard
    toEnum 2 = MDF Alternate
    toEnum _ = undefined

-- | Either a list of 'Component's for a Brassica wordlist file, or a
-- list of 'SFM' fields for an MDF file
data ParseOutput a = ParsedRaw [Component a] | ParsedMDF SFM
    deriving (Show, Functor, Foldable, Traversable)

-- | Given the selected input and output modes, and the expanded sound
-- changes, tokenise the input according to the format which was selected
tokeniseAccordingToInputFormat
    :: InputLexiconFormat
    -> OutputMode
    -> SoundChanges Expanded GraphemeList
    -> String
    -> Either (ParseErrorBundle String Void) [Component PWord]
tokeniseAccordingToInputFormat Raw _ cs =
    withFirstCategoriesDecl tokeniseWords cs
tokeniseAccordingToInputFormat (MDF h) MDFOutputWithEtymons cs =
    let h' = case h of
            Standard -> mdfHierarchy
            Alternate -> mdfAlternateHierarchy
    in
        withFirstCategoriesDecl tokeniseMDF cs <=<
        fmap (fromTree . duplicateEtymologies ('*':) . toTree h')
        . parseSFM ""
tokeniseAccordingToInputFormat (MDF _) o cs = \input -> do
    sfm <- parseSFM "" input
    ws <- withFirstCategoriesDecl tokeniseMDF cs sfm
    pure $ case o of
        MDFOutput -> ws
        _ ->
            -- need to extract words for other output modes
            -- also add separators to keep words apart visually
            intersperse (Separator "\n") $ Word <$> getWords ws

-- | Top-level dispatcher for an interactive frontend: given a textual
-- wordlist and a list of sound changes, returns the result of running
-- the changes in the specified mode.
parseTokeniseAndApplyRules
    :: (forall a b. (a -> b) -> [Component a] -> [Component b])  -- ^ mapping function to use (for parallelism)
    -> SoundChanges Expanded GraphemeList -- ^ changes
    -> String       -- ^ words
    -> InputLexiconFormat
    -> ApplicationMode
    -> Maybe [Component PWord]  -- ^ previous results
    -> ApplicationOutput PWord (Statement Expanded GraphemeList)
parseTokeniseAndApplyRules parFmap statements ws intype mode prev =
    case tokeniseAccordingToInputFormat intype (getOutputMode mode) statements ws of
        Left e -> ParseError e
        Right toks -> case mode of
            ReportRules ReportApplied ->
                AppliedRulesTable $ concat $
                    getWords $ parFmap (applyChanges statements) toks
            ReportRules ReportNotApplied ->
                let is = intersect' $ getWords $ parFmap (rulesNotApplied statements) toks
                in NotAppliedRulesList $ (statements !!) <$> is
            ApplyRules DifferentToLastRun mdfout sep ->
                let result = concatMap (splitMultipleResults sep) $
                        joinComponents' mdfout $ parFmap (doApply mdfout statements) toks
                in HighlightedWords $
                    mapMaybe polyDiffToHighlight $ getDiff (fromMaybe [] prev) result
                    -- zipWithComponents result (fromMaybe [] prev) [] $ \thisWord prevWord ->
                    --     (thisWord, thisWord /= prevWord)
            ApplyRules (DifferentToInput m) mdfout sep ->
                HighlightedWords $ concatMap (splitMultipleResults sep) $
                        joinComponents' mdfout $ parFmap (doApplyWithChanges m mdfout statements) toks
            ApplyRules NoHighlight mdfout sep ->
                HighlightedWords $ (fmap.fmap) (,False) $ concatMap (splitMultipleResults sep) $
                    joinComponents' mdfout $ parFmap (doApply mdfout statements) toks
  where
    -- highlight words in 'Second' but not 'First'
    polyDiffToHighlight :: PolyDiff (Component a) (Component a) -> Maybe (Component (a, Bool))
    polyDiffToHighlight (First _) = Nothing
    polyDiffToHighlight (Second (Word a)) = Just $ Word (a, True)
    polyDiffToHighlight (Second c) = Just $ unsafeCastComponent c
    polyDiffToHighlight (Both _ (Word a)) = Just $ Word (a, False)
    polyDiffToHighlight (Both _ c) = Just $ unsafeCastComponent c

    unsafeCastComponent :: Component a -> Component b
    unsafeCastComponent (Word _) = error "unsafeCastComponent: attempted to cast a word!"
    unsafeCastComponent (Separator s) = Separator s
    unsafeCastComponent (Gloss s) = Gloss s

    intersect' :: Eq a => [[a]] -> [a]
    intersect' [] = []
    intersect' xs = foldr1 intersect xs

    doApply :: OutputMode -> SoundChanges Expanded GraphemeList -> PWord -> [Component [PWord]]
    doApply WordsWithProtoOutput scs w = doApplyWithProto scs w
    doApply WordsWithProtoOutputPreserve scs w = doApplyWithProto scs w
    doApply _ scs w = [Word $ mapMaybe getOutput $ applyChanges scs w]

    doApplyWithProto scs w =
        let intermediates :: [[PWord]]
            intermediates = fmap nubOrd $ transpose $ getReports <$> applyChanges scs w
        in intersperse (Separator " → ") (fmap Word intermediates)

    doApplyWithChanges :: A.HighlightMode -> OutputMode -> SoundChanges Expanded GraphemeList -> PWord -> [Component [(PWord, Bool)]]
    doApplyWithChanges m WordsWithProtoOutput scs w = doApplyWithChangesWithProto m scs w
    doApplyWithChanges m WordsWithProtoOutputPreserve scs w = doApplyWithChangesWithProto m scs w
    doApplyWithChanges m _ scs w = [Word $ mapMaybe (getChangedOutputs m) $ applyChanges scs w]

    doApplyWithChangesWithProto m scs w =
        let intermediates :: [[(PWord, Bool)]]
            intermediates = fmap nubOrd $ transpose $ getChangedReports m <$> applyChanges scs w
        in intersperse (Separator " → ") (fmap Word intermediates)

    joinComponents' WordsWithProtoOutput =
        joinComponents . intersperse (Separator "\n") . filter (\case Word _ -> True; _ -> False)
    joinComponents' WordsWithProtoOutputPreserve = joinComponents . linespace
    joinComponents' _ = joinComponents

    -- Insert newlines as necessary to put each 'Word' on a separate line
    linespace :: [Component a] -> [Component a]
    linespace (Separator s:cs)
        | '\n' `elem` s = Separator s : linespace cs
        | otherwise = Separator ('\n':s) : linespace cs
    linespace (c:cs@(Separator _:_)) = c : linespace cs
    linespace (c:cs) = c : Separator "\n" : linespace cs
    linespace [] = []
