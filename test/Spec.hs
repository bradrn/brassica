{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Test.Tasty ( defaultMain, testGroup, TestTree )
import qualified Data.ByteString as B
import qualified Data.Text as T
import Brassica.SoundChange.Parse (parseSoundChanges, errorBundlePretty)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Brassica.SoundChange.Tokenise (detokeniseWords)
import Test.Tasty.Golden (goldenVsFile)
import Brassica.SoundChange (applyChanges, tokeniseAnd)
import Conduit
import System.IO (IOMode(..), hPutStrLn, withFile)
import qualified Data.ByteString.UTF8 as B8

main :: IO ()
main = defaultMain $ testGroup "brassica-tests"
     [ proto21eTest
     ]

proto21eTest :: TestTree
proto21eTest = goldenVsFile "proto21e golden test" "test/proto21e.golden" "test/proto21e.out" $ do
    withFile "test/proto21e.out" WriteMode $ \outFile -> fmap (either id id) . runExceptT $ do
        let writeLn = liftIO . hPutStrLn outFile
        soundChangeData <- B8.toString <$> liftIO (B.readFile "test/proto21e.bsc")
        soundChanges <- catchEither (parseSoundChanges soundChangeData) $ \err -> do
            writeLn "Cannot parse the SCA file because: "
            writeLn $ errorBundlePretty err
            throwE ()
        let prettyError  = B8.fromString . (++"\n") . ("SCA Error: " ++ ) . errorBundlePretty
        let prettyOutput = B8.fromString . (++"\n") . ("SCA Output: " ++ ) . detokeniseWords
        let evolve = either prettyError prettyOutput . tokeniseAnd applyChanges soundChanges
        liftIO $ withSourceFile "test/proto21e.in" $ flip connect
            $ decodeUtf8C
            .| linesUnboundedC
            .| mapC (evolve . T.unpack)
            .| sinkHandle outFile
    --, withResource (getNumCapabilities <* setNumCapabilities 1) setNumCapabilities $ const $ testGroup "benchmarks"
    --    [env (B.readFile "test/proto21e.bsc") $ bench "parsing SCA file" . nf (parseSoundChanges . B8.toString)
    --    ]
catchEither :: Applicative f => Either e a -> (e -> f a) -> f a
catchEither val f = either f pure val