{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}

module BrassicaInterop where

import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.IORef
import Data.List (intercalate)
import Foreign.C
import Foreign.StablePtr

import SoundChange
import SoundChange.Parse
import SoundChange.Types
import Data.Maybe (fromMaybe, mapMaybe)

parseTokeniseAndApplyRules_hs
    :: CString     -- ^ categories
    -> CString     -- ^ rules
    -> CString     -- ^ words
    -> CBool       -- ^ report rules applied?
    -> CInt        -- ^ highlighting mode
    -> StablePtr (IORef (Maybe [Component [Grapheme]]))  -- ^ previous results
    -> IO CString  -- ^ output (either wordlist or parse error)
parseTokeniseAndApplyRules_hs catsRaw rulesRaw wsRaw (CBool report) hlMode prevPtr = do
    catsText <- peekCString catsRaw
    rulesText <- peekCString rulesRaw
    wsText <- peekCString wsRaw

    prevRef <- deRefStablePtr prevPtr

    let cats = parseCategoriesSpec $ lines catsText

    case parseRules cats rulesText of
        Left e -> newCString $ errorBundlePretty e
        Right rules ->
            if report == 1 then do
                let result = tokeniseAnd applyRulesWithLog cats rules wsText
                writeIORef prevRef Nothing
                newCString $ intercalate "<br>" $ concat (getWords result) <&> \RuleApplied{..} ->
                    "<b>" ++ concat input ++ "</b> &rarr;\
                    \<b>" ++ concat output ++ "</b> (" ++ plaintext rule ++ ")"
            else case hlMode of
                1 -> do
                    let result = tokeniseAnd applyRules cats rules wsText
                    prev <- readIORef prevRef
                    writeIORef prevRef $ Just result
                    newCString $ escape $ detokeniseWords' id $
                        zipWithComponents result (fromMaybe [] prev) [] $ \thisWord prevWord ->
                            let thisWordStr = concat thisWord in
                                if thisWord == prevWord
                                then thisWordStr
                                else "<b>" ++ thisWordStr ++ "</b>"
                2 -> do
                    let result = tokeniseAnd applyRulesWithChanges cats rules wsText
                    writeIORef prevRef $ Just $ (fmap.fmap) fst result
                    newCString $ escape $ flip detokeniseWords' result $ \case
                        (w, False) -> concat w
                        (w, True) -> "<b>" ++ concat w ++ "</b>"
                _ -> do
                    let result = tokeniseAnd applyRules cats rules wsText
                    writeIORef prevRef $ Just result
                    newCString $ escape $ detokeniseWords result
  where
    escape :: String -> String
    escape = concatMap $ \case
        '\n' -> "<br/>"
        -- '\t' -> "&#9;"  -- this doesn't seem to do anything - keeping it here in case I eventually figure out how to do tabs in Qt
        c    -> pure c

initResults :: IO (StablePtr (IORef (Maybe [Component [Grapheme]])))
initResults = newIORef Nothing >>= newStablePtr

foreign export ccall parseTokeniseAndApplyRules_hs
    :: CString
    -> CString
    -> CString
    -> CBool
    -> CInt
    -> StablePtr (IORef (Maybe [Component [Grapheme]]))
    -> IO CString

foreign export ccall initResults :: IO (StablePtr (IORef (Maybe [Component [Grapheme]])))
