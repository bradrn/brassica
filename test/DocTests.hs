{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Traversable (for)
import Text.Pandoc
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Providers

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Brassica.SoundChange (applyChanges)
import Brassica.SoundChange.Category (expandSoundChanges)
import Brassica.SoundChange.Parse (parseSoundChanges, errorBundlePretty)
import Brassica.SoundChange.Tokenise
    ( withFirstCategoriesDecl,
      tokeniseWord,
      concatWithBoundary )

main :: IO ()
main = defaultMain $ testGroup "brassica-doctests"
    [ doctest "docs/Writing-Sound-Changes.md" ""
    , doctest "docs/Reference.md" "\
\categories noreplace\n\
\C = m n p t ch k b d j g f s sh h v z r l w y\n\
\-Stress = a e i o u\n\
\+Stress = á é í ó ú\n\
\auto -Stress\n\
\V = &&Stress\n\
\end\n"
    ]

doctest :: FilePath -> String -> TestTree
doctest file prefix = singleTest file $ DocTest file prefix

data DocTest = DocTest FilePath String

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
                            ->
                                let rule = prefix ++ T.unpack text
                                    examples = flip mapMaybe (T.lines text) $ \x ->
                                        case words $ T.unpack x of
                                            ";":i:"→":o:_ -> Just (i, o)
                                            _ -> Nothing
                                in Just (rule, examples)
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
