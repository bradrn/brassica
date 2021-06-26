{-# LANGUAGE LambdaCase   #-}

module Main where

import Control.Exception (Exception)
import Data.Bifunctor (first, bimap)
import System.Environment (getArgs)

import Conduit
import qualified Data.ByteString as B
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)

import Brassica.SoundChange
import Brassica.SoundChange.Parse
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types (Grapheme)
import Data.Conduit.Combinators (iterM)


main :: IO ()
main = do
    [inputFile] <- getArgs
    changesText <- unpack . decodeUtf8 <$> B.readFile inputFile

    case parseSoundChanges changesText of
        Left err ->
            putStrLn $ errorBundlePretty err
        Right rules ->
            runConduit $ processWords $ first errorBundlePretty . tokeniseAnd applyChanges rules

processWords
    :: (MonadIO m, MonadThrow m)
    => (String -> Either String [Component [Grapheme]])
    -> ConduitT () Void m ()
processWords evolve = stdinC
    .| iterM (liftIO . print)
    .| decodeUtf8C
    .| linesUnboundedC
    .| iterM (liftIO . print)
    .| mapC (bimap ParseException (pack . detokeniseWords) . evolve . unpack)
    .| throwOnLeft
    .| iterM (liftIO . print)
    .| unlinesC
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
