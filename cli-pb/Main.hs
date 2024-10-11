{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Conduit
import qualified Data.ByteString as B
import Data.Foldable (toList)
import qualified Data.Text as T
import Data.Text (unpack, pack, Text)
import Data.Text.Encoding (decodeUtf8)
import Options.Applicative

import Brassica.Paradigm

main :: IO ()
main = execParser opts >>= \Options{..} -> do
    paradigmText <-
        case paradigm of
            FromFile paradigmFile -> unpack . decodeUtf8 <$> B.readFile paradigmFile
            FromEval s -> pure s

    case parseParadigm paradigmText of
        Left e -> putStrLn $ errorBundlePretty e
        Right p ->
            withSourceFileIf inRootsFile $ \inC ->
            withSinkFileIf outWordsFile $ \outC ->
            runConduit $
                inC
                .| decodeUtf8C
                .| linesUnboundedC
                .| mapC (processRoot nestedOutput p)
                .| unlinesC
                .| encodeUtf8C
                .| outC
  where
    opts = info (args <**> helper <**> simpleVersioner "v1.0.0") fullDesc

    args = Options
        <$> asum
            [ FromEval <$> strOption
                (long "eval" <> short 'e' <> help "Literal paradigm to run through (newline-separated, as in paradigm file)")
            , FromFile <$> strArgument
                (metavar "PARADIGM" <> help "File containing paradigm")
            ]
        <*> switch (long "nest" <> short 'n' <> help "Print output in nested format")
        <*> optional (strOption
            (long "in" <> short 'i' <> help "File containing input roots (if not specified will read from stdin)"))
        <*> optional (strOption
            (long "out" <> short 'o' <> help "File to which output words should be written (if not specified will write to stdout)"))

    -- duplicated from main CLI
    withSourceFileIf :: Maybe FilePath -> (ConduitM i B.ByteString IO () -> IO a) -> IO a
    withSourceFileIf = maybe ($ stdinC) withSourceFile

    withSinkFileIf :: Maybe FilePath -> (ConduitM B.ByteString o IO () -> IO a) -> IO a
    withSinkFileIf = maybe ($ stdoutC) withSinkFile

    processRoot :: Bool -> Paradigm -> Text -> Text
    processRoot nestedOutput p r =
        let output = applyParadigm p $ unpack r
        in if nestedOutput
            then addSpaces output $ pack $ formatNested id output
            else T.unlines $ pack <$> toList output

    -- careful: need to add spaces to the end of each item if making nested output
    -- so that adjacent items don’t run together!
    addSpaces :: ResultsTree a -> Text -> Text
    addSpaces t = flip T.append $ T.replicate (depth t) "\n"

data ParadigmInput = FromFile String | FromEval String
    deriving (Show)

data Options = Options
    { paradigm :: ParadigmInput
    , nestedOutput :: Bool
    , inRootsFile :: Maybe String
    , outWordsFile :: Maybe String
    }
    deriving (Show)
