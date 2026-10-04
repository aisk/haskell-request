{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.HTTP.Request.Internal.Body
  ( FromResponse (..),
    ToRequestBody (..),
    ToForm (..),
    Form (..),
    ResponseBodyException (..),
    bufferResponse,
    decodeResponse,
  )
where

import Control.Exception (Exception, finally, throwIO)
import Control.Monad (void)
import Data.Aeson (AesonException (..), FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Request.Internal.Charset (charsetFromHeaders, decodeText)
import Network.HTTP.Request.Internal.Sse (feedSse, newSseParser)
import Network.HTTP.Request.Internal.Types
  ( Response (Response),
    SseEvent,
    StreamBody (..),
    responseBody,
    responseHeaders,
    responseStatus,
  )
import Network.HTTP.Types.URI (renderSimpleQuery)

newtype ResponseBodyException = ResponseBodyException String
  deriving (Show)

instance Exception ResponseBodyException

class FromResponse a where
  -- | Build the body value from a response whose body has not been read yet.
  -- An instance that does not hand the stream over to the caller must close
  -- it, which 'bufferResponse' and 'decodeResponse' do.
  fromResponse :: Response (StreamBody BS.ByteString) -> IO a

-- | Read the whole body into memory and close the stream.
bufferResponse :: Response (StreamBody BS.ByteString) -> IO (Response LBS.ByteString)
bufferResponse res = do
  chunks <- readAll `finally` closeStream stream
  return $ Response (responseStatus res) (responseHeaders res) (LBS.fromChunks chunks)
  where
    stream = responseBody res
    readAll = readNext stream >>= maybe (return []) (\chunk -> (chunk :) <$> readAll)

-- | Buffer the response and decode it with a pure function, throwing
-- 'ResponseBodyException' on failure.
decodeResponse :: (Response LBS.ByteString -> Either String a) -> Response (StreamBody BS.ByteString) -> IO a
decodeResponse decode res =
  bufferResponse res >>= either (throwIO . ResponseBodyException) return . decode

instance FromResponse BS.ByteString where
  fromResponse = fmap (LBS.toStrict . responseBody) . bufferResponse

instance FromResponse LBS.ByteString where
  fromResponse = fmap responseBody . bufferResponse

instance FromResponse T.Text where
  fromResponse res = do
    buffered <- bufferResponse res
    decodeText (charsetFromHeaders (responseHeaders buffered)) (LBS.toStrict (responseBody buffered))

instance FromResponse String where
  fromResponse = fmap T.unpack . fromResponse

instance FromResponse () where
  fromResponse = void . bufferResponse

instance {-# OVERLAPPABLE #-} (FromJSON a) => FromResponse a where
  fromResponse res = do
    buffered <- bufferResponse res
    either (throwIO . AesonException) return (eitherDecode (responseBody buffered))

instance FromResponse (StreamBody BS.ByteString) where
  fromResponse = return . responseBody

instance FromResponse (StreamBody SseEvent) where
  fromResponse res = do
    stateRef <- newIORef (newSseParser, [])
    let stream = responseBody res
        nextEvent = do
          (parser, queued) <- readIORef stateRef
          case queued of
            event : rest -> do
              writeIORef stateRef (parser, rest)
              return (Just event)
            [] -> do
              mChunk <- readNext stream
              case mChunk of
                Nothing -> return Nothing
                Just chunk -> do
                  writeIORef stateRef (feedSse parser chunk)
                  nextEvent
    return $ StreamBody nextEvent (closeStream stream)

-- TODO: When request bodies are reworked for file, multipart and streaming
-- uploads, turn this into a single-method ToRequest class, mirroring
-- FromResponse.
class ToRequestBody a where
  toRequestBody :: a -> BS.ByteString
  requestContentType :: a -> Maybe BS.ByteString
  requestContentType _ = Nothing

instance ToRequestBody BS.ByteString where
  toRequestBody = id
  requestContentType _ = Just "application/octet-stream"

instance ToRequestBody LBS.ByteString where
  toRequestBody = LBS.toStrict
  requestContentType _ = Just "application/octet-stream"

instance ToRequestBody T.Text where
  toRequestBody = T.encodeUtf8
  requestContentType _ = Just "text/plain; charset=utf-8"

instance ToRequestBody String where
  toRequestBody = T.encodeUtf8 . T.pack
  requestContentType _ = Just "text/plain; charset=utf-8"

instance {-# OVERLAPPABLE #-} (ToJSON a) => ToRequestBody a where
  toRequestBody = LBS.toStrict . encode
  requestContentType _ = Just "application/json"

instance ToRequestBody () where
  toRequestBody () = BS.empty
  requestContentType () = Nothing

class ToForm a where
  toForm :: a -> [(BS.ByteString, BS.ByteString)]

instance (k ~ BS.ByteString, v ~ BS.ByteString) => ToForm [(k, v)] where
  toForm = id

newtype Form a = Form a

instance (ToForm a) => ToRequestBody (Form a) where
  toRequestBody (Form a) = renderSimpleQuery False (toForm a)
  requestContentType _ = Just "application/x-www-form-urlencoded"
