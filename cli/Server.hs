{-# LANGUAGE OverloadedStrings #-}

module Server (serve) where

import Data.Aeson (Result(..), encode, fromJSON)
import Conduit (runConduit, (.|), stdinC, stdoutC, mapMC)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson.Parser (json')
import Data.ByteString (toStrict)
import Data.Conduit.Attoparsec (conduitParser)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(NoBuffering))
import System.Timeout

import Brassica.SoundChange.Frontend.Internal (dispatch, Response(RespError), reqTimeout)

serve :: IO ()
serve = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    runConduit $
        stdinC
        .| conduitParser json'
        .| mapMC (action . snd)
        .| stdoutC
  where
    action req' = fmap ((<> "\ETB") . toStrict . encode) $
        case fromJSON req' of
            Error e -> pure $ RespError [] e
            Success req -> do
                result <-
                    timeout (reqTimeout req) $
                    evaluate $ force $
                    dispatch req
                pure $ case result of
                    Nothing -> RespError [] "&lt;timeout&gt;"
                    Just resp -> resp
