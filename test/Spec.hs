{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Conduit
import Control.Category ((>>>))
import Control.Monad.Trans.Except (runExceptT, throwE)
import System.IO (IOMode(..), hPutStrLn, withFile)
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.Golden (goldenVsFile)

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B8
import qualified Data.Text as T

import Brassica.SoundChange (applyChanges)
import Brassica.SoundChange.Parse (parseSoundChanges, errorBundlePretty)
import Brassica.SoundChange.Tokenise (tokeniseWords, detokeniseWords, withFirstCategoriesDecl)

main :: IO ()
main = defaultMain $ testGroup "brassica-tests"
     [ proto21eTest
     ]

proto21eTest :: TestTree
proto21eTest = goldenVsFile "proto21e golden test" "test/proto21e.golden" "test/proto21e.out" $
    withFile "test/proto21e.out" WriteMode $ \outFile -> fmap (either id id) . runExceptT $ do
        let writeLn = liftIO . hPutStrLn outFile
        soundChangeData <- B8.toString <$> liftIO (B.readFile "test/proto21e.bsc")
        soundChanges <- catchEither (parseSoundChanges soundChangeData) $ \err -> do
            writeLn "Cannot parse the SCA file because: "
            writeLn $ errorBundlePretty err
            throwE ()
        let prettyError  = B8.fromString . (++"\n") . ("SCA Error: " ++ ) . errorBundlePretty
        let prettyOutput = B8.fromString . (++"\n") . ("SCA Output: " ++ ) . detokeniseWords
        let evolve =
                withFirstCategoriesDecl tokeniseWords soundChanges
                >>> (fmap.fmap.fmap) (applyChanges soundChanges)
                >>> either prettyError prettyOutput
        liftIO $ withSourceFile "test/proto21e.in" $ flip connect
            $ decodeUtf8C
            .| linesUnboundedC
            .| mapC (evolve . T.unpack)
            .| sinkHandle outFile

catchEither :: Applicative f => Either e a -> (e -> f a) -> f a
catchEither val f = either f pure val
