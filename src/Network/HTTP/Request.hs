{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.HTTP.Request
  ( Header,
    Headers,
    FromResponseBody (..),
    ToRequestBody (..),
    Manager,
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
    newManager,
    send,
    sendWith,
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
import Network.HTTP.Client (Manager)
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

findEventSep :: BS.ByteString -> Maybe (Int, Int)
findEventSep bs =
  foldr
    earlier
    Nothing
    [ tryFind "\r\n\r\n" 4,
      tryFind "\n\n" 2,
      tryFind "\r\r" 2
    ]
  where
    tryFind pat sepLen =
      let (h, t) = BS.breakSubstring pat bs
       in if BS.null t then Nothing else Just (BS.length h, BS.length h + sepLen)
    earlier a Nothing = a
    earlier Nothing b = b
    earlier a@(Just (s1, _)) b@(Just (s2, _)) = if s1 <= s2 then a else b

parseSseField :: T.Text -> Maybe (T.Text, T.Text)
parseSseField line
  | T.null line = Nothing
  | T.head line == ':' = Nothing
  | otherwise =
      let (name, rest) = T.breakOn ":" line
          value
            | T.null rest = ""
            | otherwise = case T.stripPrefix " " (T.drop 1 rest) of
                Just v -> v
                Nothing -> T.drop 1 rest
       in Just (name, value)

parseSseBlock :: BS.ByteString -> SseEvent
parseSseBlock block =
  let txt = T.replace "\r" "\n" . T.replace "\r\n" "\n" $ T.decodeUtf8Lenient block
      ls = T.lines txt
      fields = mapMaybe parseSseField ls
      dataVal = T.intercalate "\n" [v | (k, v) <- fields, k == "data"]
      typeVal = listToMaybe [v | (k, v) <- fields, k == "event"]
      idVal = listToMaybe [v | (k, v) <- fields, k == "id"]
   in SseEvent dataVal typeVal idVal

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
              return $ Just (parseSseBlock (BS.take blockEnd buf))
            Nothing -> do
              chunk <- LowLevelClient.brRead (LowLevelClient.responseBody llres)
              if BS.null chunk
                then
                  if BS.null buf
                    then return Nothing
                    else do
                      writeIORef bufRef BS.empty
                      return $ Just (parseSseBlock buf)
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

methodToByteString :: Method -> BS.ByteString
methodToByteString DELETE = "DELETE"
methodToByteString GET = "GET"
methodToByteString HEAD = "HEAD"
methodToByteString OPTIONS = "OPTIONS"
methodToByteString PATCH = "PATCH"
methodToByteString POST = "POST"
methodToByteString PUT = "PUT"
methodToByteString TRACE = "TRACE"
methodToByteString (Method m) = C.pack m

data Request a = Request
  { method :: Method,
    url :: String,
    headers :: Headers,
    body :: a
  }
  deriving (Show)

-- Compatibility accessor functions
requestMethod :: Request a -> Method
requestMethod req = req.method

requestUrl :: Request a -> String
requestUrl req = req.url

requestHeaders :: Request a -> Headers
requestHeaders req = req.headers

requestBody :: Request a -> a
requestBody req = req.body

toLowlevelRequest :: (ToRequestBody a) => Request a -> IO LowLevelClient.Request
toLowlevelRequest req = do
  initReq <- LowLevelClient.parseRequest req.url
  let autoContentType = requestContentType req.body
      hasContentType = any (\(k, _) -> k == "Content-Type") req.headers
      hasUserAgent = any (\(k, _) -> CI.mk k == CI.mk ("User-Agent" :: BS.ByteString)) req.headers
      defaultUserAgent = C.pack $ "haskell-request/" <> VERSION_request
      extraContentType =
        maybe [] (\c -> [("Content-Type", c)]) $
          if hasContentType then Nothing else autoContentType
      extraUserAgent =
        if hasUserAgent
          then []
          else [("User-Agent", defaultUserAgent)]
  return $
    initReq
      { LowLevelClient.method = methodToByteString req.method,
        LowLevelClient.requestHeaders = map (\(k, v) -> (CI.mk k, v)) (req.headers ++ extraContentType ++ extraUserAgent),
        LowLevelClient.requestBody = LowLevelClient.RequestBodyBS (toRequestBody req.body)
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

newManager :: IO Manager
newManager = LowLevelTLSClient.newTlsManager

sendWith :: (ToRequestBody a, FromResponseBody b) => Manager -> Request a -> IO (Response b)
sendWith manager req = do
  llreq <- toLowlevelRequest req
  buildResponse llreq manager

send :: (ToRequestBody a, FromResponseBody b) => Request a -> IO (Response b)
send req = do
  manager <- LowLevelTLSClient.getGlobalManager
  sendWith manager req

get :: (FromResponseBody a) => String -> IO (Response a)
get url =
  send $ Request GET url [] ()

delete :: (FromResponseBody a) => String -> IO (Response a)
delete url =
  send $ Request DELETE url [] ()

post :: (ToRequestBody a, FromResponseBody b) => String -> a -> IO (Response b)
post url body =
  send $ Request POST url [] body

put :: (ToRequestBody a, FromResponseBody b) => String -> a -> IO (Response b)
put url body =
  send $ Request PUT url [] body

patch :: (ToRequestBody a, FromResponseBody b) => String -> a -> IO (Response b)
patch url body =
  send $ Request PATCH url [] body
