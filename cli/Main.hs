{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (Exception)

import Conduit
import qualified Data.ByteString as B
import Data.Text (pack, unpack, snoc, unsnoc, Text)
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
            Right scs ->
                case expandSoundChanges scs of
                    Left err -> putStrLn $ case err of
                        (NotFound s) -> "Could not find category: " ++ s
                        InvalidBaseValue -> "Invalid value used as base grapheme in feature definition"
                        InvalidDerivedValue -> "Invalid value used as derived grapheme in autosegment"
                        MismatchedLengths -> "Mismatched lengths in feature definition"
                    Right rules' ->
                        withSourceFileIf inWordsFile $ \inC ->
                        withSinkFileIf outWordsFile $ \outC ->
                        runConduit $
                            inC
                            .| processWords (incrFor wordsFormat outMode) rules' wordsFormat outMode
                            .| outC

  where
    opts = info (args <**> helper <**> simpleVersioner "v1.0.0") fullDesc

    args = batchArgs <|> serverArgs
    serverArgs = flag' Server (long "server" <> help "Run server (for internal use only)")
    batchArgs = Options
        <$> asum
            [ FromEval <$> strOption
                (long "eval" <> short 'e' <> help "Literal sound change(s) to evaluate (newline-separated, as in rules file)")
            , FromFile <$> strArgument
                (metavar "RULES" <> help "File containing sound changes")
            ]
        <*> asum
            [ flag Raw (MDF Standard)
                (long "mdf" <> help "Parse input words in MDF format (standard hierarchy)")
            , flag Raw (MDF Alternate)
                (long "mdf-alt" <> help "Parse input words in MDF format (alternate hierarchy)")
            ]
        <*> (asum
                [ flag' (const $ ReportRules ReportApplied)
                    (long "report" <> help "Report rules applied rather than outputting words")
                , flag' (const $ ReportRules ReportNotApplied)
                    (long "report-not" <> help "Report rules which didn't apply rather than outputting words")
                , flag' (ApplyRules NoHighlight MDFOutput)
                    (long "mdf-out" <> help "With --mdf, output MDF dictionary")
                , flag' (ApplyRules NoHighlight MDFOutputWithEtymons)
                    (long "etymons" <> help "With --mdf, output MDF dictionary with etymologies")
                , flag' (ApplyRules NoHighlight WordsWithProtoOutput)
                    (long "show-input" <> help "Output an input→output wordlist")
                , flag' (ApplyRules (DifferentToInput AllChanged) WordsOnlyOutput)
                    (long "show-changed" <> help "Add [+] after all words different to input")
                , flag' (ApplyRules (DifferentToInput SpecificRule) WordsOnlyOutput)
                    (long "show-changed-h" <> help "Add [+] after all words affected by rules with -h")
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

    incrFor (MDF _) _ = False
    incrFor Raw (ReportRules ReportNotApplied) = False
    incrFor Raw _ = True

    -- duplicated in paradigm builder CLI
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
    -> SoundChanges Expanded GraphemeList
    -> InputLexiconFormat
    -> ApplicationMode
    -> ConduitT B.ByteString B.ByteString m ()
processWords incr rules wordsFormat outMode =
    decodeUtf8C
    .| (if incr then linesUnboundedC else mapC id)
    .| mapC (processApplicationOutput . evolve . unpack)
    .| throwOnLeft
    .| encodeUtf8C
  where
    evolve ws = parseTokeniseAndApplyRules parFmap rules ws wordsFormat outMode Nothing

    throwOnLeft :: (MonadThrow m, Exception e) => ConduitT (Either e r) r m ()
    throwOnLeft = awaitForever $ \case
        Left e -> throwM e
        Right r -> yield r

    processApplicationOutput :: ApplicationOutput PWord (Statement Expanded GraphemeList) -> Either ParseException Text
    processApplicationOutput (HighlightedWords cs) = Right $ ensureNewline $ pack $ detokeniseWords' highlight cs
    processApplicationOutput (AppliedRulesTable is) = Right $ pack $ unlines $ reportAsText plaintext' <$> is
    processApplicationOutput (NotAppliedRulesList is) = Right $ pack $ unlines $ plaintext' <$> is
    processApplicationOutput (ParseError e) = Left $ ParseException $ errorBundlePretty e

    ensureNewline t = case unsnoc t of
        Just (_, '\n') -> t
        _ -> snoc t '\n'

    highlight (w, False) = concatWithBoundary w
    highlight (w, True) = concatWithBoundary w ++ " [+]"

newtype ParseException = ParseException String
    deriving Show

instance Exception ParseException
