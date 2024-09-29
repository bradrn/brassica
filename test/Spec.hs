{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Conduit
import Control.Category ((>>>))
import Control.Monad.Trans.Except (runExceptT, throwE)
import System.IO (IOMode(..), withFile)
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.Golden (goldenVsFile)

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B8
import qualified Data.Text as T

import Brassica.SoundChange (applyChanges, splitMultipleResults, applyChangesWithLogs, reportAsText)
import Brassica.SoundChange.Expand (expandSoundChanges)
import Brassica.SoundChange.Parse (parseSoundChanges, errorBundlePretty)
import Brassica.SoundChange.Tokenise (tokeniseWords, detokeniseWords, withFirstCategoriesDecl, Component, getWords)
import Brassica.SoundChange.Types (SoundChanges, PWord, plaintext', Expanded, Grapheme)

main :: IO ()
main = defaultMain $ testGroup "brassica-tests"
    [ changesTest applyChanges showWord "changes golden test" "words.out" "words.golden"
    , changesTest applyChangesWithLogs showLogs "changes golden test with log" "words-log.out" "words-log.golden"
    ]
  where
    showWord = detokeniseWords . concatMap (splitMultipleResults " ")

    showLogs logs = unlines $ fmap (reportAsText plaintext') $ concat $ getWords logs

changesTest
    :: (SoundChanges Expanded (Bool, [Grapheme]) -> PWord -> [a])
    -> ([Component [a]] -> String)
    -> String
    -> FilePath
    -> FilePath
    -> TestTree
changesTest applyFn showWord testName outName goldenName =
    let outName' = "test/" ++ outName
        goldenName' = "test/" ++ goldenName
    in goldenVsFile testName goldenName' outName' $
        withFile outName' WriteMode $ \outFile -> fmap (either id id) . runExceptT $ do
            soundChangesData <- B8.toString <$> liftIO (B.readFile "test/changes.bsc")
            soundChanges' <- catchEither (parseSoundChanges soundChangesData) $ \err -> do
                liftIO $ putStrLn $
                    "Cannot parse the SCA file because:\n" ++
                    errorBundlePretty err
                throwE ()
            soundChanges <- catchEither (expandSoundChanges soundChanges') $ \err -> do
                liftIO $ putStrLn $
                    "Cannot expand the SCA file because:\n" ++
                    show err
                throwE ()
            let output pre = B8.fromString . (++"\n") . (pre ++)
                prettyError  = output "SCA Error:" . errorBundlePretty
                prettyOutput = output "SCA Output: " . showWord
                evolve =
                    withFirstCategoriesDecl tokeniseWords soundChanges
                    >>> (fmap.fmap.fmap) (applyFn soundChanges)
                    >>> either prettyError prettyOutput
            liftIO $ withSourceFile "test/words.in" $ flip connect
                $ decodeUtf8C
                .| linesUnboundedC
                .| mapC (evolve . T.unpack)
                .| sinkHandle outFile


catchEither :: Applicative f => Either e a -> (e -> f a) -> f a
catchEither val f = either f pure val
