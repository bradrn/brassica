{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module BrassicaInterop where

import Data.Aeson (encode, decodeStrict)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad ((<=<))
import Data.ByteString (ByteString, packCStringLen, toStrict)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Foreign
import Foreign.C hiding (newCString, peekCString) -- hide these so we don't accidentally use them
import Foreign.Ptr (Ptr)
import Foreign.StablePtr
import System.Timeout

import Brassica.SoundChange.Frontend.Internal


------ ByteString decoding: modified from /u/vdukhovni
-- see https://www.reddit.com/r/haskell/comments/mxyt9j/comment/gvy3rsy

foreign import ccall unsafe "string.h memcpy" memcpy ::
    CString -> CString -> CSize -> IO (Ptr ())

-- | Returns a NUL-terminated CString, internal NULs not supported.
copyCStringLen :: CStringLen -> IO CStringLen
copyCStringLen (str, len) = do
    buf <- Foreign.mallocBytes $ 1 + len
    _ <- memcpy buf str $ fromIntegral len
    Foreign.pokeElemOff buf len 0
    return (buf, len)

copyByteString :: ByteString -> IO CStringLen
copyByteString bs = unsafeUseAsCStringLen bs copyCStringLen

------

newStableCStringLen :: ByteString -> IO (StablePtr CStringLen)
newStableCStringLen = newStablePtr <=< copyByteString

getString :: StablePtr CStringLen -> IO CString
getString = fmap fst . deRefStablePtr

getStringLen :: StablePtr CStringLen -> IO Int
getStringLen = fmap snd . deRefStablePtr

freeStableCStringLen :: StablePtr CStringLen -> IO ()
freeStableCStringLen ptr = do
    (cstr, _) <- deRefStablePtr ptr
    Foreign.free cstr
    freeStablePtr ptr

dispatch_hs :: CString -> Int -> IO (StablePtr CStringLen)
dispatch_hs reqRaw reqRawLen = do
    reqText <- packCStringLen (reqRaw, reqRawLen)
    response <-
        case decodeStrict reqText of
            Nothing -> pure $ RespError [] "dispatch_hs: error"
            Just req -> do
                result <-
                    timeout (reqTimeout req) $
                    evaluate $ force $
                    dispatch req
                pure $ case result of
                    Nothing -> RespError [] "&lt;timeout&gt;"
                    Just resp -> resp
    newStableCStringLen $ toStrict $ encode response

foreign export ccall dispatch_hs :: CString -> Int -> IO (StablePtr CStringLen)
foreign export ccall getString :: StablePtr CStringLen -> IO CString
foreign export ccall getStringLen :: StablePtr CStringLen -> IO Int
foreign export ccall freeStableCStringLen :: StablePtr CStringLen -> IO ()
