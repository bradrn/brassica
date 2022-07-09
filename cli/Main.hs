{-# LANGUAGE LambdaCase   #-}

module Main where

import Control.Category ((>>>))
import Control.Exception (Exception)
import Data.Bifunctor (bimap)
import System.Environment (getArgs)

import Conduit
import qualified Data.ByteString as B
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)

import Brassica.SoundChange
import Brassica.SoundChange.Parse
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types (Grapheme)


main :: IO ()
main = do
    (inputFile, wordsFormat) <- decodeArgs <$> getArgs
    changesText <- unpack . decodeUtf8 <$> B.readFile inputFile

    case parseSoundChanges changesText of
        Left err ->
            putStrLn $ errorBundlePretty err
        Right rules ->
            runConduit $ processWords (incrFor wordsFormat) $
                tokeniseAccordingToInputFormat wordsFormat Normal rules
                >>> (fmap.fmap) (applyChanges rules)
                >>> bimap errorBundlePretty (componentise MDFOutput)
  where
    decodeArgs [inputFile] = (inputFile, Raw)
    decodeArgs ["--mdf", inputFile] = (inputFile, MDF)
    decodeArgs [inputFile, "--mdf"] = (inputFile, MDF)
    decodeArgs _ = error "Command-line arguments could not be parsed"

    incrFor Raw = True
    incrFor MDF = False

processWords
    :: (MonadIO m, MonadThrow m)
    => Bool  -- split into lines?
    -> (String -> Either String [Component [Grapheme]])
    -> ConduitT () Void m ()
processWords incr evolve = stdinC
    .| decodeUtf8C
    .| (if incr then linesUnboundedC else mapC id)
    .| mapC (bimap ParseException (pack . detokeniseWords) . evolve . unpack)
    .| throwOnLeft
    .| (if incr then unlinesC else mapC id)
    .| encodeUtf8C
    .| stdoutC
  where
    throwOnLeft :: (MonadThrow m, Exception e) => ConduitT (Either e r) r m ()
    throwOnLeft = awaitForever $ \case
        Left e -> throwM e
        Right r -> yield r

newtype ParseException = ParseException String
    deriving Show

instance Exception ParseException
