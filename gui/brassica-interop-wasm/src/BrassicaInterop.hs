{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module BrassicaInterop where

import Control.Monad ((<=<))
import Data.IORef
import Data.Foldable (toList)
import qualified Foreign
import Foreign.C hiding (newCString, peekCString) -- hide these so we don't accidentally use them
import Foreign.StablePtr
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)

import Brassica.Paradigm
import Brassica.SoundChange
import Brassica.SoundChange.Frontend.Internal

newStableCStringLen :: String -> IO (StablePtr CStringLen)
newStableCStringLen = newStablePtr <=< GHC.newCStringLen utf8

getString :: StablePtr CStringLen -> IO CString
getString = fmap fst . deRefStablePtr

getStringLen :: StablePtr CStringLen -> IO Int
getStringLen = fmap snd . deRefStablePtr

freeStableCStringLen :: StablePtr CStringLen -> IO ()
freeStableCStringLen ptr = do
    (cstr, _) <- deRefStablePtr ptr
    Foreign.free cstr
    freeStablePtr ptr

parseTokeniseAndApplyRules_hs
    :: CString     -- ^ changes
    -> Int         -- ^ length of changes
    -> CString     -- ^ words
    -> Int         -- ^ length of words
    -> CString     -- ^ separator
    -> Int         -- ^ length of separator
    -> CBool       -- ^ report rules applied?
    -> CInt        -- ^ input format
    -> CInt        -- ^ highlighting mode
    -> CInt        -- ^ output mode
    -> StablePtr (IORef (Maybe [Component PWord]))  -- ^ previous results
    -> IO (StablePtr CStringLen)  -- ^ output (either wordlist or parse error)
parseTokeniseAndApplyRules_hs
  changesRaw
  changesRawLen
  wsRaw
  wsRawLen
  sepRaw
  sepRawLen
  (CBool report)
  infmtC
  hlModeC
  outModeC
  prevPtr
  = do
    changesText <- GHC.peekCStringLen utf8 (changesRaw, changesRawLen)
    wsText      <- GHC.peekCStringLen utf8 (wsRaw, wsRawLen)
    sepText     <- GHC.peekCStringLen utf8 (sepRaw, sepRawLen)

    prevRef <- deRefStablePtr prevPtr
    prev <- readIORef prevRef

    let hlMode = toEnum $ fromIntegral hlModeC
        infmt = toEnum $ fromIntegral infmtC
        outMode = toEnum $ fromIntegral outModeC
        mode =
            if report == 1
            then ReportRulesApplied
            else ApplyRules hlMode outMode sepText

    case parseSoundChanges changesText of
        Left e -> newStableCStringLen $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
        Right statements ->
            case expandSoundChanges statements of
                Left err ->
                    newStableCStringLen $ ("<pre>"++) $ (++"</pre>") $ case err of
                        (NotFound s) -> "Could not find category: " ++ s
                        InvalidBaseValue -> "Invalid value used as base grapheme in feature definition"
                        MismatchedLengths -> "Mismatched lengths in feature definition"
                        InvalidDerivedValue -> "Invalid value used as derived grapheme in autosegment"
                        InvalidAuto s -> "Invalid category name used for autosegment: " ++ s
                Right statements' ->
                    case parseTokeniseAndApplyRules (fmap.fmap) statements' wsText infmt mode prev of
                        ParseError e -> newStableCStringLen $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
                        HighlightedWords result -> do
                            writeIORef prevRef $ Just $ (fmap.fmap) fst result
                            newStableCStringLen $ escape $ detokeniseWords' highlightWord result
                        AppliedRulesTable items -> do
                            writeIORef prevRef Nothing
                            newStableCStringLen $
                                concatMap (surroundTable . reportAsHtmlRows plaintext') items
  where
    highlightWord (s, False) = concatWithBoundary s
    highlightWord (s, True) = "<b>" ++ concatWithBoundary s ++ "</b>"

    surroundTable :: String -> String
    surroundTable s = "<table>" ++ s ++ "</table>"

initResults :: IO (StablePtr (IORef (Maybe [Component PWord])))
initResults = newIORef Nothing >>= newStablePtr

escape :: String -> String
escape = concatMap $ \case
    '\n' -> "<br/>"
    -- '\t' -> "&#9;"  -- this doesn't seem to do anything - keeping it here in case I eventually figure out how to do tabs in Qt
    c    -> pure c

parseAndBuildParadigm_hs
    :: CString  -- ^ paradigm text
    -> Int      -- ^ length of paradigm text
    -> CString  -- ^ input words
    -> Int      -- ^ length of input words
    -> CBool    -- ^ separate lines? / non-compact?
    -> IO (StablePtr CStringLen)
parseAndBuildParadigm_hs
  paradigmRaw
  paradigmRawLen
  inputRaw
  inputRawLen
  (CBool separateLines)
  = do
    paradigmText <- GHC.peekCStringLen utf8 (paradigmRaw, paradigmRawLen)
    inputText <- GHC.peekCStringLen utf8 (inputRaw, inputRawLen)

    newStableCStringLen $ case parseParadigm paradigmText of
        Left e -> "<pre>" ++ errorBundlePretty e ++ "</pre>"
        Right p -> escape $
            (if separateLines == 1
                then unlines . toList
                else formatNested id)
            $ Node $ applyParadigm p <$> lines inputText

foreign export ccall parseTokeniseAndApplyRules_hs
    :: CString
    -> Int
    -> CString
    -> Int
    -> CString
    -> Int
    -> CBool
    -> CInt
    -> CInt
    -> CInt
    -> StablePtr (IORef (Maybe [Component PWord]))
    -> IO (StablePtr CStringLen)

foreign export ccall parseAndBuildParadigm_hs
    :: CString
    -> Int
    -> CString
    -> Int
    -> CBool
    -> IO (StablePtr CStringLen)

foreign export ccall initResults :: IO (StablePtr (IORef (Maybe [Component PWord])))
foreign export ccall getString :: StablePtr CStringLen -> IO CString
foreign export ccall getStringLen :: StablePtr CStringLen -> IO Int
foreign export ccall freeStableCStringLen :: StablePtr CStringLen -> IO ()
