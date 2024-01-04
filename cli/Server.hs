{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Server (serve) where

import Conduit (runConduit, (.|), stdinC, stdoutC, mapMC)
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate)
import Data.Aeson (Result(..), encode, fromJSON)
import Data.Aeson.Parser (json')
import Data.Aeson.TH (deriveJSON, defaultOptions, defaultTaggedObject, constructorTagModifier, sumEncoding, tagFieldName)
import Data.ByteString (toStrict)
import Data.Conduit.Attoparsec (conduitParser)
import Data.Foldable (toList)
import GHC.Generics (Generic)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(NoBuffering))
import System.Timeout

import Brassica.SoundChange
import Brassica.SoundChange.Frontend.Internal
import Brassica.Paradigm (applyParadigm, parseParadigm, formatNested, ResultsTree (..))

data Request
    = ReqRules
        { changes :: String
        , input :: String
        , report :: Bool
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

$(deriveJSON defaultOptions ''Component)
$(deriveJSON defaultOptions ''Grapheme)
$(deriveJSON defaultOptions ''InputLexiconFormat)
$(deriveJSON defaultOptions ''HighlightMode)
$(deriveJSON defaultOptions ''OutputMode)

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
    let mode =
            if report
            then ReportRulesApplied
            else ApplyRules hlMode outMode sep
    in case parseSoundChanges changes of
        Left e -> RespError $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
        Right statements ->
            let result' = parseTokeniseAndApplyRules statements input inFmt mode prev
            in case result' of
                ParseError e -> RespError $
                    "<pre>" ++ errorBundlePretty e ++ "</pre>"
                HighlightedWords result -> RespRules
                    (Just $ (fmap.fmap) fst result)
                    (escape $ detokeniseWords' highlightWord result)
                AppliedRulesTable items -> RespRules Nothing $
                    surroundTable $ concatMap (reportAsHtmlRows plaintext') items
                ExpandError err -> RespError $ ("<pre>"++) $ (++"</pre>") $ case err of
                    (NotFound s) -> "Could not find category: " ++ s
                    InvalidBaseValue -> "Invalid value used as base grapheme in feature definition"
                    MismatchedLengths -> "Mismatched lengths in feature definition"
  where
    highlightWord (s, False) = concatWithBoundary s
    highlightWord (s, True) = "<b>" ++ concatWithBoundary s ++ "</b>"

    surroundTable :: String -> String
    surroundTable s = "<table>" ++ s ++ "</table>"
parseTokeniseAndApplyRulesWrapper _ = error "parseTokeniseAndApplyRulesWrapper: unexpected request!"

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
