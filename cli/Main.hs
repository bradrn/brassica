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
            withSourceFileIf inWordsFile $ \inC ->
            withSinkFileIf outWordsFile $ \outC ->
            let c e = inC .| processWords (incrFor wordsFormat) e .| outC
            in runConduit $ c $
                tokeniseAccordingToInputFormat wordsFormat Normal rules
                >>> (fmap.fmap) (applyChanges rules)
                >>> bimap errorBundlePretty (componentise MDFOutput)
  where
    opts = info (args <**> helper) fullDesc

    args = Options
        <$> strArgument
            (metavar "RULES" <> help "File containing sound changes")
        <*> flag Raw MDF
            (long "mdf" <> help "Parse input words in MDF format")
        <*> optional (strOption
            (long "in" <> short 'i' <> help "File containing input words (if not specified will read from stdin)"))
        <*> optional (strOption
            (long "out" <> short 'o' <> help "File to which output words should be written (if not specified will write to stdout)"))

    incrFor Raw = True
    incrFor MDF = False

    withSourceFileIf :: Maybe FilePath -> (ConduitM i B.ByteString IO () -> IO a) -> IO a
    withSourceFileIf = maybe ($ stdinC) withSourceFile

    withSinkFileIf :: Maybe FilePath -> (ConduitM B.ByteString o IO () -> IO a) -> IO a
    withSinkFileIf = maybe ($ stdoutC) withSinkFile

data Options = Options
    { rulesFile :: String
    , wordsFormat :: InputLexiconFormat
    , inWordsFile :: Maybe String
    , outWordsFile :: Maybe String
    } deriving (Show)

processWords
    :: (MonadIO m, MonadThrow m)
    => Bool  -- split into lines?
    -> (String -> Either String [Component [Grapheme]])
    -> ConduitT B.ByteString B.ByteString m ()
processWords incr evolve =
    decodeUtf8C
    .| (if incr then linesUnboundedC else mapC id)
    .| mapC (bimap ParseException (pack . detokeniseWords) . evolve . unpack)
    .| throwOnLeft
    .| (if incr then unlinesC else mapC id)
    .| encodeUtf8C
  where
    throwOnLeft :: (MonadThrow m, Exception e) => ConduitT (Either e r) r m ()
    throwOnLeft = awaitForever $ \case
        Left e -> throwM e
        Right r -> yield r

newtype ParseException = ParseException String
    deriving Show

instance Exception ParseException
