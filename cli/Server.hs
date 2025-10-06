{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Server (serve, parFmap) where

import Conduit (runConduit, (.|), stdinC, stdoutC, mapMC)
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate)
import Control.Parallel.Strategies (withStrategy, parTraversable, rseq)
import Data.Aeson (Result(..), encode, FromJSON(..), ToJSON(..), Value (..), fromJSON)
import Data.Aeson.Parser (json')
import Data.Aeson.TH (deriveJSON, defaultOptions, defaultTaggedObject, constructorTagModifier, sumEncoding, tagFieldName)
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.ByteString (toStrict)
import Data.Conduit.Attoparsec (conduitParser)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Text (unpack)
import GHC.Generics (Generic)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(NoBuffering))
import System.Timeout

import Brassica.SoundChange hiding (HighlightMode)
import Brassica.SoundChange.Frontend.Internal
import Brassica.Paradigm (applyParadigm, parseParadigm, formatNested, ResultsTree (..))

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
        }
    | ReqParadigm
        { pText :: String
        , input :: String
        , separateLines :: Bool
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
    | RespError String
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

$(deriveJSON defaultOptions ''Component)
$(deriveJSON defaultOptions ''OutputMode)
$(deriveJSON defaultOptions ''ReportMode)

$(deriveJSON defaultOptions{constructorTagModifier=drop 3, sumEncoding=defaultTaggedObject{tagFieldName="method"}} ''Request)
$(deriveJSON defaultOptions{constructorTagModifier=drop 4, sumEncoding=defaultTaggedObject{tagFieldName="method"}} ''Response)

serve :: IO ()
serve = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    runConduit $
        stdinC
        .| conduitParser json'
        .| mapMC (action . snd)
        .| stdoutC
  where
    action req' = fmap ((<> "\ETB") . toStrict . encode) $
        case fromJSON req' of
            Error e -> pure $ RespError e
            Success req -> do
                result <-
                    timeout 5000000 $  -- 5 s
                    evaluate $ force $
                    dispatch req
                pure $ case result of
                    Nothing -> RespError "&lt;timeout&gt;"
                    Just resp -> resp

dispatch :: Request -> Response
dispatch r@(ReqRules{}) = parseTokeniseAndApplyRulesWrapper r
dispatch r@(ReqParadigm{}) = parseAndBuildParadigmWrapper r

parseTokeniseAndApplyRulesWrapper
    :: Request
    -> Response
parseTokeniseAndApplyRulesWrapper ReqRules{..} =
    let mode = maybe (ApplyRules hlMode outMode sep) ReportRules report
    in case parseSoundChanges changes of
        Left e -> RespError $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
        Right statements ->
            case expandSoundChanges statements of
                Left err -> RespError $ ("<pre>"++) $ (++"</pre>") $ case err of
                    (NotFound s) -> "Could not find category: " ++ s
                    InvalidBaseValue -> "Invalid value used as base grapheme in feature definition"
                    InvalidDerivedValue -> "Invalid value used as derived grapheme in autosegment"
                    MismatchedLengths -> "Mismatched lengths in feature definition"
                Right statements' ->
                    let result' = parseTokeniseAndApplyRules parFmap statements' input inFmt mode prev
                    in case result' of
                        ParseError e -> RespError $
                            "<pre>" ++ errorBundlePretty e ++ "</pre>"
                        HighlightedWords result -> RespRules
                            (Just $ (fmap.fmap) fst result)
                            (escape $ detokeniseWords' highlightWord result)
                        AppliedRulesTable items -> RespRules Nothing $
                            concatMap (surroundTable . reportAsHtmlRows plaintext') items
                        NotAppliedRulesList items -> RespRules Nothing $
                            intercalate "<br/>" $ plaintext' <$> items
  where
    highlightWord (s, False) = concatWithBoundary s
    highlightWord (s, True) = "<b>" ++ concatWithBoundary s ++ "</b>"

    surroundTable :: String -> String
    surroundTable s = "<table>" ++ s ++ "</table>"
parseTokeniseAndApplyRulesWrapper _ = error "parseTokeniseAndApplyRulesWrapper: unexpected request!"

parFmap :: (a -> b) -> [Component a] -> [Component b]
parFmap f = withStrategy (parTraversable rseq) . fmap (fmap f)

parseAndBuildParadigmWrapper :: Request -> Response
parseAndBuildParadigmWrapper ReqParadigm{..} =
    case parseParadigm pText of
        Left e -> RespError $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
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
