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

import Brassica.SoundChange (applyChanges, concatWithBoundary, getOutput)
import Brassica.SoundChange.Expand (expandSoundChanges)
import Brassica.SoundChange.Parse (parseSoundChanges, errorBundlePretty)
import Brassica.SoundChange.Tokenise (withFirstCategoriesDecl, tokeniseWord)

main :: IO ()
main = do
    t1 <- doctest "docs/Writing-Sound-Changes.md" ""
    t2 <- doctest "docs/Reference.md" "\
\categories noreplace\n\
\C = m n p t ch k b d j g f s sh h v z r l w y\n\
\-Stress = a e i o u\n\
\+Stress = á é í ó ú\n\
\auto -Stress\n\
\V = &&Stress\n\
\end\n"
    defaultMain $ testGroup "brassica-doctests" [t1, t2]

doctest :: FilePath -> String -> IO TestTree
doctest path prefix = do
    doc <- T.readFile path
    tests <- runIO (readMarkdown def { readerExtensions = githubMarkdownExtensions } doc) <&> \case
        Left err -> error $ T.unpack $ renderError err
        Right (Pandoc _ blocks) ->
            flip mapMaybe (zip [(0::Int)..] blocks) $ \case
                    (i, CodeBlock (_, cs, _) text)
                        | "brassica" `elem` cs -> Just $
                          singleTest (show i) $ DocTest (T.pack prefix) text
                    _ -> Nothing
    pure $ testGroup path tests

data DocTest = DocTest T.Text T.Text

instance IsTest DocTest where
    testOptions = pure []
    run _ (DocTest prefix text) _ = pure $
        let rule = T.unpack (prefix <> text)
            examples = flip mapMaybe (T.lines text) $ \x ->
                case words $ T.unpack x of
                    ";":i:"→":o:_ -> Just (i, o)
                    _ -> Nothing
            results = case parseSoundChanges rule of
                Left err -> Left $ errorBundlePretty err
                Right scs ->
                    case expandSoundChanges scs of
                        Left err -> Left $ show err
                        Right scs' -> for examples $ \(i, o) ->
                            case withFirstCategoriesDecl tokeniseWord scs' i of
                                Left err -> Left $ errorBundlePretty err
                                Right i' ->
                                    let o' = mapMaybe getOutput $ applyChanges scs' i'
                                        o'' =
                                            if null o'
                                            then "(deleted)"
                                            else intercalate "/" $ concatWithBoundary <$> o'
                                    in if o == o''
                                            then Right ("" :: String)
                                            else Left $
                                                    "expected " ++ i ++ "→" ++ o ++
                                                    ", got " ++ o'' ++
                                                    "\nfor sound change:\n" ++ rule
        in case results of
            Left err -> testFailed err
            Right _ -> testPassed ""
