{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (Exception)

import Conduit
import qualified Data.ByteString as B
import Data.Text (pack, unpack, snoc, Text)
import Data.Text.Encoding (decodeUtf8)
import Options.Applicative

import Brassica.SoundChange
import Brassica.SoundChange.Frontend.Internal
import Server

main :: IO ()
main = execParser opts >>= \case
    Server -> serve
    Options{..} -> do
        changesText <-
            case rules of
                FromFile rulesFile -> unpack . decodeUtf8 <$> B.readFile rulesFile
                FromEval s -> pure s

        case parseSoundChanges changesText of
            Left err ->
                putStrLn $ errorBundlePretty err
            Right rules ->
                case expandSoundChanges rules of
                    Left err -> putStrLn $ case err of
                        (NotFound s) -> "Could not find category: " ++ s
                        InvalidBaseValue -> "Invalid value used as base grapheme in feature definition"
                        MismatchedLengths -> "Mismatched lengths in feature definition"
                    Right rules' ->
                        withSourceFileIf inWordsFile $ \inC ->
                        withSinkFileIf outWordsFile $ \outC ->
                        runConduit $
                            inC
                            .| processWords (incrFor wordsFormat) rules' wordsFormat outMode
                            .| outC

  where
    opts = info (args <**> helper <**> simpleVersioner "v0.2.0") fullDesc

    args = batchArgs <|> serverArgs
    serverArgs = flag' Server (long "server" <> help "Run server (for internal use only)")
    batchArgs = Options
        <$> asum
            [ FromEval <$> strOption
                (long "eval" <> short 'e' <> help "Literal sound change(s) to evaluate (newline-separated, as in rules file)")
            , FromFile <$> strArgument
                (metavar "RULES" <> help "File containing sound changes")
            ]
        <*> flag Raw MDF
            (long "mdf" <> help "Parse input words in MDF format")
        <*> (asum
                [ flag' (const ReportRulesApplied)
                    (long "report" <> help "Report rules applied rather than outputting words")
                , flag' (ApplyRules NoHighlight MDFOutput)
                    (long "mdf-out" <> help "With --mdf, output MDF dictionary")
                , flag' (ApplyRules NoHighlight MDFOutputWithEtymons)
                    (long "etymons" <> help "With --mdf, output MDF dictionary with etymologies")
                , flag' (ApplyRules NoHighlight WordsWithProtoOutput)
                    (long "show-input" <> help "Output an input→output wordlist")
                , flag
                    (ApplyRules NoHighlight WordsOnlyOutput)
                    (ApplyRules NoHighlight WordsOnlyOutput)
                    (long "wordlist" <> help "Output only a list of the derived words (default)")
                ]
            <*> strOption
                (long "separator" <> short 's' <> value "/" <> help "Separator between multiple results (default: /)"))
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

data Rules = FromFile String | FromEval String
    deriving (Show)

data Options = Options
    { rules :: Rules
    , wordsFormat :: InputLexiconFormat
    , outMode :: ApplicationMode
    , inWordsFile :: Maybe String
    , outWordsFile :: Maybe String
    }
    | Server
    deriving (Show)

processWords
    :: (MonadIO m, MonadThrow m)
    => Bool  -- split into lines?
    -> SoundChanges Expanded [Grapheme]
    -> InputLexiconFormat
    -> ApplicationMode
    -> ConduitT B.ByteString B.ByteString m ()
processWords incr rules wordsFormat outMode =
    decodeUtf8C
    .| (if incr then linesUnboundedC else mapC id)
    .| mapC (processApplicationOutput . evolve . unpack . (`snoc` '\n'))
    .| throwOnLeft
    .| encodeUtf8C
  where
    evolve ws = parseTokeniseAndApplyRules parFmap rules ws wordsFormat outMode Nothing

    throwOnLeft :: (MonadThrow m, Exception e) => ConduitT (Either e r) r m ()
    throwOnLeft = awaitForever $ \case
        Left e -> throwM e
        Right r -> yield r

    processApplicationOutput :: ApplicationOutput PWord (Statement Expanded [Grapheme]) -> Either ParseException Text
    processApplicationOutput (HighlightedWords cs) = Right $ pack $ detokeniseWords $ (fmap.fmap) fst cs
    processApplicationOutput (AppliedRulesTable is) = Right $ pack $ unlines $ reportAsText plaintext' <$> is
    processApplicationOutput (ParseError e) = Left $ ParseException $ errorBundlePretty e

newtype ParseException = ParseException String
    deriving Show

instance Exception ParseException
