{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Test.Tasty ( defaultMain, testGroup, TestTree )
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Brassica.SoundChange.Parse (parseSoundChanges, errorBundlePretty)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Brassica.SoundChange.Tokenise (detokeniseWords)
import Test.Tasty.Golden (goldenVsFile)
import Brassica.SoundChange (applyChanges, tokeniseAnd)
import Conduit
import System.IO (IOMode(..), hPutStrLn, withFile)
import Control.Monad.IO.Class ( MonadIO(liftIO) )

main :: IO ()
main = defaultMain $ testGroup "brassica-tests"
     [ proto21eTest
     ]

proto21eTest :: TestTree
proto21eTest = goldenVsFile "proto21e test" "test/proto21e.golden" "test/proto21e.out" $ do
    withFile "test/proto21e.out" WriteMode $ \outFile -> fmap (either id id) . runExceptT $ do
        let writeLn = liftIO . hPutStrLn outFile
        soundChangeData <- T.unpack . T.decodeUtf8 <$> liftIO (B.readFile "test/proto21e.bsc")
        soundChanges <- catchEither (parseSoundChanges soundChangeData) $ \err -> do
            writeLn "Cannot parse the SCA file because: "
            writeLn $ errorBundlePretty err
            throwE ()
        let prettyError  = T.encodeUtf8 . T.pack . (++"\n") . ("SCA Error: " ++ ) . errorBundlePretty
        let prettyOutput = T.encodeUtf8 . T.pack . (++"\n") . ("SCA Output: " ++ ) . detokeniseWords
        let evolve = either prettyError prettyOutput . tokeniseAnd applyChanges soundChanges
        liftIO $ withSourceFile "test/proto21e.in" $ flip connect
             $ decodeUtf8C
            .| linesUnboundedC
            .| mapC (evolve . T.unpack)
            .| sinkHandle outFile
    where
        catchEither (Left  a) f = f a
        catchEither (Right b) _ = pure b