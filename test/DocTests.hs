{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Category ((>>>))
import System.IO (IOMode(..), withFile)
import Text.Pandoc
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.Providers

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Brassica.SoundChange (applyChanges, splitMultipleResults, applyChangesWithLogs, reportAsText)
import Brassica.SoundChange.Category (expandSoundChanges)
import Brassica.SoundChange.Parse (parseSoundChanges, errorBundlePretty)
import Brassica.SoundChange.Tokenise (tokeniseWords, detokeniseWords, withFirstCategoriesDecl, Component, getWords, tokeniseWord)
import Brassica.SoundChange.Types (SoundChanges, PWord, plaintext', Expanded, Grapheme)
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Debug.Trace
import Data.Traversable (for)
import Data.List (intersperse, intercalate)
import Brassica.SoundChange.Tokenise (concatWithBoundary)

main :: IO ()
main = defaultMain $ testGroup "brassica-doctests"
    [ doctest "docs/Reference.md" $ Just "\
\categories noreplace\n\
\C = m n p t ch k b d j g f s sh h v z r l w y\n\
\-Stress = a e i o u\n\
\+Stress = á é í ó ú\n\
\auto -Stress\n\
\V = &&Stress\n\
\end\n"
    ]

doctest :: FilePath -> Maybe String -> TestTree
doctest file prefix = singleTest file $ DocTest file prefix

data DocTest = DocTest FilePath (Maybe String)

instance IsTest DocTest where
    testOptions = pure []
    run _ (DocTest path prefix) _ = do
        doc <- T.readFile path
        runIO (readMarkdown def { readerExtensions = githubMarkdownExtensions } doc) <&> \case
            Left err -> testFailed $ T.unpack $ renderError err
            Right (Pandoc _ blocks) ->
                let rules = flip mapMaybe blocks $ \case
                        CodeBlock (_, cs, _) text
                            | "brassica" `elem` cs
                            , rule:xs <- T.lines text
                            ->
                                let rule' = maybe id (++) prefix $ T.unpack rule
                                    examples = flip mapMaybe xs $ \x ->
                                        case words $ T.unpack x of
                                            ";":i:"→":o:_ -> Just (i, o)
                                            _ -> Nothing
                                in Just (rule', examples)
                        _ -> Nothing
                    results = for rules $ \(rule, examples) ->
                        case parseSoundChanges rule of
                            Left err -> Left $ errorBundlePretty err
                            Right scs ->
                                case expandSoundChanges scs of
                                    Left err -> Left $ show err
                                    Right scs' -> for examples $ \(i, o) ->
                                        case withFirstCategoriesDecl tokeniseWord scs' i of
                                            Left err -> Left $ errorBundlePretty err
                                            Right i' ->
                                                let o' = applyChanges scs' i'
                                                    o'' = intercalate "/" $ concatWithBoundary <$> o'
                                                in if o == o''
                                                      then Right ("" :: String)
                                                      else Left $
                                                               "expected " ++ i ++ "→" ++ o ++
                                                               ", got " ++ o'' ++
                                                               "\nfor sound change:\n" ++ rule
                in case results of
                    Left err -> testFailed err
                    Right _ -> testPassed ""
