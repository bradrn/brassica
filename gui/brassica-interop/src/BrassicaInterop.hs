{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}

module BrassicaInterop where

import Data.IORef
import Foreign.C
import Foreign.StablePtr

import SoundChange
import SoundChange.Parse
import SoundChange.Types
import Data.Maybe (fromMaybe)
import Data.ByteString (packCString)
import qualified Data.ByteString.UTF8 as B8

parseTokeniseAndApplyRules_hs
    :: CString     -- ^ changes
    -> CString     -- ^ words
    -> CBool       -- ^ report rules applied?
    -> CInt        -- ^ highlighting mode
    -> StablePtr (IORef (Maybe [Component [Grapheme]]))  -- ^ previous results
    -> IO CString  -- ^ output (either wordlist or parse error)
parseTokeniseAndApplyRules_hs changesRaw wsRaw (CBool report) hlMode prevPtr = do
    changesText <- B8.toString <$> packCString changesRaw
    wsText    <- B8.toString <$> packCString wsRaw

    prevRef <- deRefStablePtr prevPtr

    case parseSoundChanges changesText of
        Left e -> newCString $ "<pre>" ++ errorBundlePretty e ++ "</pre>"
        Right statements ->
            if report == 1 then do
                let result = tokeniseAnd applyChangesWithLog statements wsText
                writeIORef prevRef Nothing
                newCString $ surroundTable $ formatLog $ concat (getWords result)
            else case hlMode of
                1 -> do
                    let result = tokeniseAnd applyChanges statements wsText
                    prev <- readIORef prevRef
                    writeIORef prevRef $ Just result
                    newCString $ escape $ detokeniseWords' id $
                        zipWithComponents result (fromMaybe [] prev) [] $ \thisWord prevWord ->
                            let thisWordStr = concat thisWord in
                                if thisWord == prevWord
                                then thisWordStr
                                else "<b>" ++ thisWordStr ++ "</b>"
                2 -> do
                    let result = tokeniseAnd applyChangesWithChanges statements wsText
                    writeIORef prevRef $ Just $ (fmap.fmap) fst result
                    newCString $ escape $ flip detokeniseWords' result $ \case
                        (w, False) -> concat w
                        (w, True) -> "<b>" ++ concat w ++ "</b>"
                _ -> do
                    let result = tokeniseAnd applyChanges statements wsText
                    writeIORef prevRef $ Just result
                    newCString $ escape $ detokeniseWords result
  where
    formatLog :: [LogItem Statement] -> String
    formatLog = concat . go Nothing
      where
        go :: Maybe [Grapheme] -> [LogItem Statement] -> [String]
        go _ [] = []
        go prev (ActionApplied{..} : ls) =
            let cell1 = case prev of
                    Just input' | input == input' -> ""
                    _ -> concat input
            in
                ("<tr><td>" ++ cell1 ++ "</td><td>&rarr;</td>\
                \<td>" ++ concat output ++ "</td><td>(" ++ plaintext' action ++ ")</td></tr>")
                : go (Just output) ls

        plaintext' :: Statement -> String
        plaintext' (RuleS r) = plaintext r
        plaintext' (CategoriesDeclS _) = "categories … end"

    surroundTable :: String -> String
    surroundTable s = "<table>" ++ s ++ "</table>"

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
    -> CBool
    -> CInt
    -> StablePtr (IORef (Maybe [Component [Grapheme]]))
    -> IO CString

foreign export ccall initResults :: IO (StablePtr (IORef (Maybe [Component [Grapheme]])))
