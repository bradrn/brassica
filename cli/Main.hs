{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Category ((>>>))
import Control.Exception (Exception)
import Data.Bifunctor (bimap)

import Conduit
import qualified Data.ByteString as B
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Options.Applicative

import Brassica.SoundChange
import Brassica.SoundChange.Parse
import Brassica.SoundChange.Tokenise
import Brassica.SoundChange.Types (Grapheme)


main :: IO ()
main = do
    Options{..} <- execParser opts
    changesText <- unpack . decodeUtf8 <$> B.readFile rulesFile

    case parseSoundChanges changesText of
        Left err ->
            putStrLn $ errorBundlePretty err
        Right rules ->
            runConduit $ processWords (incrFor wordsFormat) $
                tokeniseAccordingToInputFormat wordsFormat Normal rules
                >>> (fmap.fmap) (applyChanges rules)
                >>> bimap errorBundlePretty (componentise MDFOutput)
  where
    opts = info (args <**> helper) fullDesc

    args = Options
        <$> flag Raw MDF
            (long "mdf" <> help "Parse input words in MDF format")
        <*> strArgument
            (metavar "RULES" <> help "File containing sound changes")

    incrFor Raw = True
    incrFor MDF = False

data Options = Options
    { wordsFormat :: InputLexiconFormat
    , rulesFile :: String
    } deriving (Show)

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
