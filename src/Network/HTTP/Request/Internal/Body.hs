{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.HTTP.Request.Internal.Body
  ( FromResponseBody (..),
    ToRequestBody (..),
    ToForm (..),
    Form (..),
    ResponseBodyException (..),
  )
where

import Control.Exception (Exception, SomeException, throwIO, toException)
import Data.Aeson (AesonException (..), FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as LowLevelClient
import Network.HTTP.Request.Internal.Sse (findEventSep, parseSseBlock)
import Network.HTTP.Request.Internal.Types (Response (..), SseEvent, StreamBody (..))
import qualified Network.HTTP.Types.Status as LowLevelStatus
import Network.HTTP.Types.URI (renderSimpleQuery)

newtype ResponseBodyException = ResponseBodyException String
  deriving (Show)

instance Exception ResponseBodyException

class FromResponseBody a where
  fromResponseBody :: LBS.ByteString -> Either String a

  responseBodyException :: proxy a -> String -> SomeException
  responseBodyException _ = toException . ResponseBodyException

  buildResponse :: LowLevelClient.Request -> LowLevelClient.Manager -> IO (Response a)
  buildResponse llreq manager = do
    llres <- LowLevelClient.httpLbs llreq manager
    case fromLowLevelResponse llres of
      Right res -> return res
      Left err -> throwIO (responseBodyException (Proxy :: Proxy a) err)

instance FromResponseBody BS.ByteString where
  fromResponseBody = Right . LBS.toStrict

instance FromResponseBody LBS.ByteString where
  fromResponseBody = Right

instance FromResponseBody T.Text where
  fromResponseBody = Right . T.decodeUtf8Lenient . LBS.toStrict

instance FromResponseBody String where
  fromResponseBody = Right . T.unpack . T.decodeUtf8Lenient . LBS.toStrict

instance {-# OVERLAPPABLE #-} (FromJSON a) => FromResponseBody a where
  fromResponseBody = eitherDecode
  responseBodyException _ = toException . AesonException

instance FromResponseBody (StreamBody BS.ByteString) where
  fromResponseBody _ = Left "StreamBody must be built via buildResponse"

  buildResponse llreq manager = do
    llres <- LowLevelClient.responseOpen llreq manager
    let status = LowLevelStatus.statusCode . LowLevelClient.responseStatus $ llres
        hdrs = map (\(k, v) -> (CI.original k, v)) (LowLevelClient.responseHeaders llres)
        readNext = do
          chunk <- LowLevelClient.brRead (LowLevelClient.responseBody llres)
          return $ if BS.null chunk then Nothing else Just chunk
    return $ Response status hdrs (StreamBody readNext (LowLevelClient.responseClose llres))

instance FromResponseBody (StreamBody SseEvent) where
  fromResponseBody _ = Left "StreamBody must be built via buildResponse"

  buildResponse llreq manager = do
    llres <- LowLevelClient.responseOpen llreq manager
    bufRef <- newIORef BS.empty
    let status = LowLevelStatus.statusCode . LowLevelClient.responseStatus $ llres
        hdrs = map (\(k, v) -> (CI.original k, v)) (LowLevelClient.responseHeaders llres)
        readNext = do
          buf <- readIORef bufRef
          case findEventSep buf of
            Just (blockEnd, afterSep) -> do
              writeIORef bufRef (BS.drop afterSep buf)
              case parseSseBlock (BS.take blockEnd buf) of
                Just event -> return (Just event)
                Nothing -> readNext
            Nothing -> do
              chunk <- LowLevelClient.brRead (LowLevelClient.responseBody llres)
              if BS.null chunk
                then
                  if BS.null buf
                    then return Nothing
                    else do
                      writeIORef bufRef BS.empty
                      return (parseSseBlock buf)
                else do
                  modifyIORef bufRef (<> chunk)
                  readNext
    return $ Response status hdrs (StreamBody readNext (LowLevelClient.responseClose llres))

class ToRequestBody a where
  toRequestBody :: a -> BS.ByteString
  requestContentType :: a -> Maybe BS.ByteString
  requestContentType _ = Nothing

instance ToRequestBody BS.ByteString where
  toRequestBody = id
  requestContentType _ = Just "text/plain; charset=utf-8"

instance ToRequestBody LBS.ByteString where
  toRequestBody = LBS.toStrict
  requestContentType _ = Just "text/plain; charset=utf-8"

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

fromLowLevelResponse :: (FromResponseBody a) => LowLevelClient.Response LBS.ByteString -> Either String (Response a)
fromLowLevelResponse res =
  let status = LowLevelStatus.statusCode . LowLevelClient.responseStatus $ res
      headers = LowLevelClient.responseHeaders res
   in case fromResponseBody $ LowLevelClient.responseBody res of
        Right body ->
          Right $
            Response
              status
              (map (\(k, v) -> (CI.original k, v)) headers)
              body
        Left err -> Left err
