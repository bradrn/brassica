{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module BrassicaInterop where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.IORef
import Foreign.C hiding (newCString, peekCString) -- hide these so we don't accidentally use them
import Foreign.StablePtr
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)
import System.Timeout

import Brassica.SoundChange
import Brassica.SoundChange.Frontend.Internal
import Brassica.Paradigm (applyParadigm, parseParadigm)

parseTokeniseAndApplyRules_hs
    :: CString     -- ^ changes
    -> CString     -- ^ words
    -> CBool       -- ^ report rules applied?
    -> CInt        -- ^ input format
    -> CInt        -- ^ highlighting mode
    -> CInt        -- ^ output mode
    -> CInt        -- ^ timeout (μs)
    -> StablePtr (IORef (Maybe [Component PWord]))  -- ^ previous results
    -> IO CString  -- ^ output (either wordlist or parse error)
parseTokeniseAndApplyRules_hs
  changesRaw
  wsRaw
  (CBool report)
  infmtC
  hlModeC
  timeoutC
  outModeC
  prevPtr
  = do
    changesText <- GHC.peekCString utf8 changesRaw
    wsText      <- GHC.peekCString utf8 wsRaw

    prevRef <- deRefStablePtr prevPtr
    prev <- readIORef prevRef

    let hlMode = toEnum $ fromIntegral hlModeC
        infmt = toEnum $ fromIntegral infmtC
        outMode = toEnum $ fromIntegral outModeC
        mode =
            if report == 1
            then ReportRulesApplied
            else ApplyRules hlMode outMode

    case parseSoundChanges changesText of
        Left e -> GHC.newCString utf8 $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
        Right statements -> do
            result' <-
                timeout (fromIntegral timeoutC) $
                    evaluate $ force $
                        parseTokeniseAndApplyRules statements wsText infmt mode prev
            case result' of
                Nothing -> GHC.newCString utf8 "&lt;timeout&gt;"
                Just (ParseError e) -> GHC.newCString utf8 $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
                Just (HighlightedWords result) -> do
                    writeIORef prevRef $ Just $ (fmap.fmap) fst result
                    GHC.newCString utf8 $ escape $ detokeniseWords' highlightWord result
                Just (AppliedRulesTable items) -> do
                    writeIORef prevRef Nothing
                    GHC.newCString utf8 $ surroundTable $
                        concatMap (reportAsHtmlRows plaintext') items
  where
    highlightWord (s, False) = concat s
    highlightWord (s, True) = "<b>" ++ concat s ++ "</b>"

    surroundTable :: String -> String
    surroundTable s = "<table>" ++ s ++ "</table>"

initResults :: IO (StablePtr (IORef (Maybe [Component PWord])))
initResults = newIORef Nothing >>= newStablePtr

parseAndBuildParadigm_hs
    :: CString -- ^ paradigm
    -> CString -- ^ roots
    -> IO CString
parseAndBuildParadigm_hs pRaw wsRaw = do
    pText <- GHC.peekCString utf8 pRaw
    wsText <- GHC.peekCString utf8 wsRaw
    case parseParadigm pText of
        Left e -> GHC.newCString utf8 $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
        Right p -> GHC.newCString utf8 $ escape $ unlines $ concatMap (applyParadigm p) $ lines wsText

escape :: String -> String
escape = concatMap $ \case
    '\n' -> "<br/>"
    -- '\t' -> "&#9;"  -- this doesn't seem to do anything - keeping it here in case I eventually figure out how to do tabs in Qt
    c    -> pure c

foreign export ccall parseTokeniseAndApplyRules_hs
    :: CString
    -> CString
    -> CBool
    -> CInt
    -> CInt
    -> CInt
    -> CInt
    -> StablePtr (IORef (Maybe [Component PWord]))
    -> IO CString

foreign export ccall initResults :: IO (StablePtr (IORef (Maybe [Component PWord])))

foreign export ccall parseAndBuildParadigm_hs
    :: CString -- ^ paradigm
    -> CString -- ^ words
    -> IO CString
