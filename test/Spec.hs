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

import Brassica.SoundChange (applyChanges, splitMultipleResults, applyChangesWithLogs, tableItemToHtmlRows)
import Brassica.SoundChange.Parse (parseSoundChanges, errorBundlePretty)
import Brassica.SoundChange.Tokenise (tokeniseWords, detokeniseWords, withFirstCategoriesDecl, Component, getWords)
import Brassica.SoundChange.Types (SoundChanges, PWord)

main :: IO ()
main = defaultMain $ testGroup "brassica-tests"
    [ proto21eTest applyChanges showWord "proto21e golden test" "proto21e.out" "proto21e.golden"
    , proto21eTest applyChangesWithLogs showLogs "proto21e golden test with log" "proto21e-log.out.html" "proto21e-log.golden"
    ]
  where
    showWord = detokeniseWords . concatMap splitMultipleResults

    showLogs logs = (("<table>"++) . (++"</table>")) $
        concatMap (tableItemToHtmlRows $ const "statement") $ concat $ getWords logs

proto21eTest
    :: (SoundChanges -> PWord -> [a])
    -> ([Component [a]] -> String)
    -> String
    -> FilePath
    -> FilePath
    -> TestTree
proto21eTest applyFn showWord testName outName goldenName =
    let outName' = "test/" ++ outName
        goldenName' = "test/" ++ goldenName
    in goldenVsFile testName goldenName' outName' $
        withFile outName' WriteMode $ \outFile -> fmap (either id id) . runExceptT $ do
            soundChangesData <- B8.toString <$> liftIO (B.readFile "test/proto21e.bsc")
            soundChanges <- catchEither (parseSoundChanges soundChangesData) $ \err -> do
                liftIO $ putStrLn $
                    "Cannot parse the SCA file because:\n" ++
                    errorBundlePretty err
                throwE ()
            let output pre = B8.fromString . (++"\n") . (pre ++)
                prettyError  = output "SCA Error:" . errorBundlePretty
                prettyOutput = output "SCA Output: " . showWord
                evolve =
                    withFirstCategoriesDecl tokeniseWords soundChanges
                    >>> (fmap.fmap.fmap) (applyFn soundChanges)
                    >>> either prettyError prettyOutput
            liftIO $ withSourceFile "test/proto21e.in" $ flip connect
                $ decodeUtf8C
                .| linesUnboundedC
                .| mapC (evolve . T.unpack)
                .| sinkHandle outFile


catchEither :: Applicative f => Either e a -> (e -> f a) -> f a
catchEither val f = either f pure val
