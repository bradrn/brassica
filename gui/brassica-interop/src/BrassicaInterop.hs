{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module BrassicaInterop where

import Data.IORef
import Foreign.C hiding (newCString, peekCString) -- hide these so we don't accidentally use them
import Foreign.StablePtr
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)

import Brassica.SoundChange
import Brassica.SoundChange.Parse
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types
import Brassica.Paradigm (build)
import Brassica.Paradigm.Parse

parseTokeniseAndApplyRules_hs
    :: CString     -- ^ changes
    -> CString     -- ^ words
    -> CBool       -- ^ report rules applied?
    -> CInt        -- ^ input format
    -> CInt        -- ^ highlighting mode
    -> CInt        -- ^ MDF output mode
    -> StablePtr (IORef (Maybe [Component [Grapheme]]))  -- ^ previous results
    -> IO CString  -- ^ output (either wordlist or parse error)
parseTokeniseAndApplyRules_hs changesRaw wsRaw (CBool report) infmtC hlModeC mdfOutC prevPtr = do
    changesText <- GHC.peekCString utf8 changesRaw
    wsText      <- GHC.peekCString utf8 wsRaw

    prevRef <- deRefStablePtr prevPtr
    prev <- readIORef prevRef

    let hlMode = toEnum $ fromIntegral hlModeC
        infmt = toEnum $ fromIntegral infmtC
        mdfOut = toEnum $ fromIntegral mdfOutC
        mode =
            if report == 1
            then ReportRulesApplied
            else ApplyRules hlMode mdfOut

    case parseSoundChanges changesText of
        Left e -> GHC.newCString utf8 $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
        Right statements ->
            case parseTokeniseAndApplyRules statements wsText infmt mode prev of
                ParseError e -> GHC.newCString utf8 $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
                HighlightedWords result -> do
                    writeIORef prevRef $ Just $ (fmap.fmap) fst result
                    GHC.newCString utf8 $ escape $ detokeniseWords' highlightWord result
                AppliedRulesTable items -> do
                    writeIORef prevRef Nothing
                    GHC.newCString utf8 $ surroundTable $
                        concatMap (tableItemToHtmlRows plaintext') items
  where
    highlightWord (s, False) = concat s
    highlightWord (s, True) = "<b>" ++ concat s ++ "</b>"

    plaintext' :: Statement -> String
    plaintext' (RuleS r) = plaintext r
    plaintext' (CategoriesDeclS _) = "categories … end"

    surroundTable :: String -> String
    surroundTable s = "<table>" ++ s ++ "</table>"

initResults :: IO (StablePtr (IORef (Maybe [Component [Grapheme]])))
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
        Right p -> GHC.newCString utf8 $ escape $ unlines $ build p $ lines wsText

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
    -> StablePtr (IORef (Maybe [Component [Grapheme]]))
    -> IO CString

foreign export ccall initResults :: IO (StablePtr (IORef (Maybe [Component [Grapheme]])))

foreign export ccall parseAndBuildParadigm_hs
    :: CString -- ^ paradigm
    -> CString -- ^ words
    -> IO CString
