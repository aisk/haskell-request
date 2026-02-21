{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.HTTP.Request
  ( Header,
    Headers,
    FromResponseBody (..),
    ToRequestBody (..),
    Method (..),
    Request (..),
    Response (..),
    StreamBody (..),
    SseEvent (..),
    get,
    delete,
    patch,
    post,
    put,
    send,
    requestMethod,
    requestUrl,
    requestHeaders,
    requestBody,
    responseStatus,
    responseHeaders,
    responseBody,
  )
where

import Control.Exception (throwIO)
import Data.Aeson (AesonException (..), FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as LowLevelClient
import qualified Network.HTTP.Client.TLS as LowLevelTLSClient
import qualified Network.HTTP.Types.Status as LowLevelStatus

type Header = (BS.ByteString, BS.ByteString)

type Headers = [Header]

class FromResponseBody a where
  fromResponseBody :: LBS.ByteString -> Either String a

  buildResponse :: LowLevelClient.Request -> LowLevelClient.Manager -> IO (Response a)
  buildResponse llreq manager = do
    llres <- LowLevelClient.httpLbs llreq manager
    case fromLowLevelResponse llres of
      Right res -> return res
      Left err -> throwIO (AesonException err)

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

data StreamBody a = StreamBody
  { readNext :: IO (Maybe a),
    closeStream :: IO ()
  }

data SseEvent = SseEvent
  { sseData :: T.Text,
    sseType :: Maybe T.Text,
    sseId :: Maybe T.Text
  }
  deriving (Show)

extractSseEvent :: T.Text -> Maybe (SseEvent, T.Text)
extractSseEvent buf =
  case T.breakOn "\n\n" buf of
    (_, rest) | T.null rest -> Nothing
    (block, rest) ->
      let remaining = T.drop 2 rest
          ls = T.lines block
          getData l = if "data: " `T.isPrefixOf` l then Just (T.drop 6 l) else Nothing
          getType l = if "event: " `T.isPrefixOf` l then Just (T.drop 7 l) else Nothing
          getId l = if "id: " `T.isPrefixOf` l then Just (T.drop 4 l) else Nothing
          dataVal = T.intercalate "\n" . mapMaybe getData $ ls
          typeVal = listToMaybe . mapMaybe getType $ ls
          idVal = listToMaybe . mapMaybe getId $ ls
       in Just (SseEvent dataVal typeVal idVal, remaining)

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
    bufRef <- newIORef ""
    let status = LowLevelStatus.statusCode . LowLevelClient.responseStatus $ llres
        hdrs = map (\(k, v) -> (CI.original k, v)) (LowLevelClient.responseHeaders llres)
        readNext = do
          chunk <- LowLevelClient.brRead (LowLevelClient.responseBody llres)
          if BS.null chunk
            then return Nothing
            else do
              modifyIORef bufRef (<> T.decodeUtf8Lenient chunk)
              buf <- readIORef bufRef
              case extractSseEvent buf of
                Just (event, rest) -> writeIORef bufRef rest >> return (Just event)
                Nothing -> readNext
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

data Method
  = DELETE
  | GET
  | HEAD
  | OPTIONS
  | PATCH
  | POST
  | PUT
  | TRACE
  | Method String
  deriving (Show, Eq)

data Request a = Request
  { method :: Method,
    url :: String,
    headers :: Headers,
    body :: Maybe a
  }
  deriving (Show)

-- Compatibility accessor functions
requestMethod :: Request a -> Method
requestMethod req = req.method

requestUrl :: Request a -> String
requestUrl req = req.url

requestHeaders :: Request a -> Headers
requestHeaders req = req.headers

requestBody :: Request a -> Maybe a
requestBody req = req.body

toLowlevelRequest :: (ToRequestBody a) => Request a -> IO LowLevelClient.Request
toLowlevelRequest req = do
  initReq <- LowLevelClient.parseRequest req.url
  let autoContentType = req.body >>= requestContentType
      hasContentType = any (\(k, _) -> k == "Content-Type") req.headers
      extraHeaders = maybe [] (\c -> [("Content-Type", c)]) $
        if hasContentType then Nothing else autoContentType
  return $
    initReq
      { LowLevelClient.method = C.pack . show $ req.method,
        LowLevelClient.requestHeaders = map (\(k, v) -> (CI.mk k, v)) (req.headers ++ extraHeaders),
        LowLevelClient.requestBody = maybe mempty (LowLevelClient.RequestBodyBS . toRequestBody) req.body
      }

data Response a = Response
  { status :: Int,
    headers :: Headers,
    body :: a
  }
  deriving (Show)

-- Compatibility accessor functions for Response
responseStatus :: Response a -> Int
responseStatus res = res.status

responseHeaders :: Response a -> Headers
responseHeaders res = res.headers

responseBody :: Response a -> a
responseBody res = res.body

fromLowLevelResponse :: (FromResponseBody a) => LowLevelClient.Response LBS.ByteString -> Either String (Response a)
fromLowLevelResponse res =
  let status = LowLevelStatus.statusCode . LowLevelClient.responseStatus $ res
      headers = LowLevelClient.responseHeaders res
   in case fromResponseBody $ LowLevelClient.responseBody res of
        Right body ->
          Right $
            Response
              status
              ( map
                  ( \(k, v) ->
                      let hk = CI.original k
                       in (hk, v)
                  )
                  headers
              )
              body
        Left err -> Left err

send :: (ToRequestBody a, FromResponseBody b) => Request a -> IO (Response b)
send req = do
  manager <- LowLevelTLSClient.getGlobalManager
  llreq <- toLowlevelRequest req
  buildResponse llreq manager

get :: (FromResponseBody a) => String -> IO (Response a)
get url =
  send $ Request GET url [] (Nothing :: Maybe BS.ByteString)

delete :: (FromResponseBody a) => String -> IO (Response a)
delete url =
  send $ Request DELETE url [] (Nothing :: Maybe BS.ByteString)

post :: (ToRequestBody a, FromResponseBody b) => String -> a -> IO (Response b)
post url body =
  send $ Request POST url [] (Just body)

put :: (ToRequestBody a, FromResponseBody b) => String -> a -> IO (Response b)
put url body =
  send $ Request PUT url [] (Just body)

patch :: (ToRequestBody a, FromResponseBody b) => String -> a -> IO (Response b)
patch url body =
  send $ Request PATCH url [] (Just body)
