{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

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
import Control.Parallel.Strategies (withStrategy, parTraversable, rseq)
import Data.Aeson (FromJSON(..), ToJSON(..), Value (..))
import Data.Aeson.TH (deriveJSON, defaultOptions, defaultTaggedObject, constructorTagModifier, sumEncoding, tagFieldName)
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List (transpose, intersperse, intersectBy)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Void (Void)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Myers.Diff (getDiff, PolyDiff(..))

import Control.DeepSeq (NFData)
import Text.Megaparsec (reachOffsetNoLine, errorOffset, PosState (..), SourcePos (..), unPos)
import Text.Megaparsec.Error (ParseErrorBundle(..))

import Brassica.SFM.MDF
import Brassica.SFM.SFM
import Brassica.SoundChange (ExpandError(..), parseSoundChanges, errorBundlePretty, expandSoundChanges)
import Brassica.SoundChange.Apply hiding (HighlightMode)
import qualified Brassica.SoundChange.Apply as A
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types
import Brassica.Paradigm (parseParadigm, formatNested, ResultsTree (..), applyParadigm)

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

-- | Mode for reporting output words (and sometimes intermediate and
-- input words too)
data OutputMode
    = MDFOutput
    | WordsOnlyOutput
    | MDFOutputWithEtymons
    | WordsWithProtoOutput
    | WordsWithProtoOutputPreserve
    deriving (Show, Eq)

-- | Output of a single application of rules to a wordlist: either a
-- list of possibly highlighted words, an applied rules table, or a
-- parse error.
data ApplicationOutput a r
    = HighlightedWords [Component (a, Bool)]
    | AppliedRulesTable [Log r]
    | NotAppliedRulesList [Rule Expanded]
    | ParseError (ParseErrorBundle String Void)
    deriving (Show, Generic, NFData)

-- | For MDF input, the hierarchy used
data MDFHierarchy = Standard | Alternate
    deriving (Show, Eq)

-- | Kind of input: either a raw wordlist, or an MDF file.
data InputLexiconFormat = Raw | MDF MDFHierarchy
    deriving (Show, Eq)

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
                let ws' = getWords $ parFmap (applyChanges statements) toks
                    na = mapMaybe tagLOC . rulesNotApplied <$> concat ws'
                in NotAppliedRulesList $ intersectByTag na
                -- let is = intersect' $ getWords $ parFmap (rulesNotApplied statements) toks
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

    intersectByTag :: Eq a => [[(a, b)]] -> [b]
    intersectByTag [] = []
    intersectByTag xs = snd <$> foldr1 (intersectBy (\x y -> fst x == fst y)) xs

    tagLOC :: Statement Expanded GraphemeList -> Maybe (Int, Rule Expanded)
    tagLOC (RuleS r) = Just (loc r, r)
    tagLOC _ = Nothing

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

getErrorLocs :: ParseErrorBundle String Void -> [Int]
getErrorLocs ParseErrorBundle { bundleErrors, bundlePosState } =
    go (NE.toList bundleErrors) bundlePosState
  where
    -- based on Text.Megaparsec.Error.errorBundlePrettyWith
    go (e:es) pst =
        let pst' = reachOffsetNoLine (errorOffset e) pst
            l = unPos $ sourceLine $ pstateSourcePos pst'
        in l : go es pst'
    go [] _ = []

x :: Int
x = length []


-------- JSON server


data Request
    = ReqRules
        { changes :: String
        , input :: String
        , report :: Maybe ReportMode
        , inFmt :: InputLexiconFormat
        , hlMode :: HighlightMode
        , outMode :: OutputMode
        , prev :: Maybe [Component PWord]
        , sep :: String
        , reqTimeout :: Int   -- ^ microseconds
        }
    | ReqParadigm
        { pText :: String
        , input :: String
        , separateLines :: Bool
        , reqTimeout :: Int  -- ^ microseconds
        }
    deriving (Show)

data Response
    = RespRules
        { prev :: Maybe [Component PWord]
        , output :: String
        }
    | RespParadigm
        { output :: String
        }
    | RespNotApplied
        { highlights :: [Int]
        }
    | RespError
        { highlights :: [Int]
        , message :: String
        }
    deriving (Show, Generic, NFData)

instance ToJSON InputLexiconFormat where
    toJSON Raw = "Raw"
    toJSON (MDF Standard) = "MDFStandard"
    toJSON (MDF Alternate) = "MDFAlternate"

instance FromJSON InputLexiconFormat where
    parseJSON (String "Raw") = pure Raw
    parseJSON (String "MDFStandard") = pure $ MDF Standard
    parseJSON (String "MDFAlternate") = pure $ MDF Alternate
    parseJSON (String s) = fail $ "Unknown InputLexiconFormat: " ++ unpack s
    parseJSON invalid = prependFailure "parsing InputLexiconFormat failed: " $
        typeMismatch "String" invalid

instance FromJSON HighlightMode where
    parseJSON (String "NoHighlight") = pure NoHighlight
    parseJSON (String "DifferentToLastRun") = pure DifferentToLastRun
    parseJSON (String "DifferentToInputAllChanged") = pure $ DifferentToInput AllChanged
    parseJSON (String "DifferentToInputSpecificRule") = pure $ DifferentToInput SpecificRule
    parseJSON invalid = prependFailure "parsing HighlightMode failed: " $
        typeMismatch "String" invalid

instance ToJSON HighlightMode where
    toJSON NoHighlight = "NoHighlight"
    toJSON DifferentToLastRun = "DifferentToLastRun"
    toJSON (DifferentToInput AllChanged) = "DifferentToInputAllChanged"
    toJSON (DifferentToInput SpecificRule) = "DifferentToInputSpecificRule"

$(deriveJSON defaultOptions ''OutputMode)
$(deriveJSON defaultOptions ''ReportMode)

$(deriveJSON defaultOptions{constructorTagModifier=drop 3, sumEncoding=defaultTaggedObject{tagFieldName="method"}} ''Request)
$(deriveJSON defaultOptions{constructorTagModifier=drop 4, sumEncoding=defaultTaggedObject{tagFieldName="method"}} ''Response)


dispatch :: Request -> Response
dispatch r = case r of
    ReqRules{} -> parseTokeniseAndApplyRulesWrapper r
    ReqParadigm{} -> parseAndBuildParadigmWrapper r
  where
    parseTokeniseAndApplyRulesWrapper
        :: Request
        -> Response
    parseTokeniseAndApplyRulesWrapper ReqRules{..} =
        let mode = maybe (ApplyRules hlMode outMode sep) ReportRules report
        in case parseSoundChanges changes of
            Left e -> RespError (getErrorLocs e) $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
            Right statements ->
                case expandSoundChanges statements of
                    Left (loc, err) -> RespError (maybeToList loc) $ ("<pre>"++) $ (++"</pre>") $ case err of
                        (NotFound s) -> "Could not find category: " ++ s
                        InvalidBaseValue -> "Invalid value used as base grapheme in feature definition"
                        InvalidDerivedValue -> "Invalid value used as derived grapheme in autosegment"
                        MismatchedLengths -> "Mismatched lengths in feature definition"
                    Right statements' ->
                        let result' = parseTokeniseAndApplyRules parFmap statements' input inFmt mode prev
                        in case result' of
                            ParseError e -> RespError [] $
                                "<pre>" ++ errorBundlePretty e ++ "</pre>"
                            HighlightedWords result -> RespRules
                                (Just $ (fmap.fmap) fst result)
                                (escape $ detokeniseWords' highlightWord result)
                            AppliedRulesTable items -> RespRules Nothing $
                                concatMap (surroundTable . reportAsHtmlRows plaintext') items
                            NotAppliedRulesList items -> RespNotApplied $ loc <$> items
      where
        highlightWord (s, False) = concatWithBoundary s
        highlightWord (s, True) = "<b>" ++ concatWithBoundary s ++ "</b>"

        surroundTable :: String -> String
        surroundTable s = "<table>" ++ s ++ "</table>"
    parseTokeniseAndApplyRulesWrapper _ = error "parseTokeniseAndApplyRulesWrapper: unexpected request!"

    parseAndBuildParadigmWrapper :: Request -> Response
    parseAndBuildParadigmWrapper ReqParadigm{..} =
        case parseParadigm pText of
            Left e -> RespError [] $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
            Right p -> RespParadigm $ escape $
                (if separateLines
                    then unlines . toList
                    else formatNested id)
                $ Node $ applyParadigm p <$> lines input
    parseAndBuildParadigmWrapper _ = error "parseAndBuildParadigmWrapper: unexpected request!"

    escape :: String -> String
    escape = concatMap $ \case
        '\n' -> "<br/>"
        -- '\t' -> "&#9;"  -- this doesn't seem to do anything - keeping it here in case I eventually figure out how to do tabs in Qt
        c    -> pure c

parFmap :: (a -> b) -> [Component a] -> [Component b]
parFmap f = withStrategy (parTraversable rseq) . fmap (fmap f)
