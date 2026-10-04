{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Request.Internal.Client
  ( Manager,
    newManager,
    send,
    sendWith,
    get,
    delete,
    patch,
    post,
    put,
  )
where

import Control.Exception (bracketOnError)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as LowLevelClient
import qualified Network.HTTP.Client.TLS as LowLevelTLSClient
import Network.HTTP.Request.Internal.Body (FromResponse (..), ToRequestBody (..))
import Network.HTTP.Request.Internal.Types
  ( Method (..),
    Request (Request),
    Response (Response),
    StreamBody (StreamBody),
    requestBody,
    requestHeaders,
    requestMethod,
    requestUrl,
  )
import qualified Network.HTTP.Types.Status as LowLevelStatus

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

toLowlevelRequest :: (ToRequestBody a) => Request a -> IO LowLevelClient.Request
toLowlevelRequest req = do
  initReq <- LowLevelClient.parseRequest (requestUrl req)
  let body = requestBody req
      headers = requestHeaders req
      autoContentType = requestContentType body
      hasContentType = any (\(k, _) -> CI.mk k == CI.mk ("Content-Type" :: BS.ByteString)) headers
      hasUserAgent = any (\(k, _) -> CI.mk k == CI.mk ("User-Agent" :: BS.ByteString)) headers
      defaultUserAgent = C.pack $ "haskell-request/" <> VERSION_request
      extraContentType =
        maybe [] (\c -> [("Content-Type", c)]) $
          if hasContentType then Nothing else autoContentType
      extraUserAgent =
        if hasUserAgent
          then []
          else [("User-Agent", defaultUserAgent)]
      newHeaders = map (\(k, v) -> (CI.mk k, v)) (headers ++ extraContentType ++ extraUserAgent)
      -- Keep headers derived from the URL (e.g. credentials) unless overridden.
      keptHeaders = filter (\(k, _) -> k `notElem` map fst newHeaders) (LowLevelClient.requestHeaders initReq)
  return $
    initReq
      { LowLevelClient.method = methodToByteString (requestMethod req),
        LowLevelClient.requestHeaders = keptHeaders ++ newHeaders,
        LowLevelClient.requestBody = LowLevelClient.RequestBodyBS (toRequestBody body)
      }

newManager :: IO Manager
newManager = LowLevelTLSClient.newTlsManager

sendWith :: (ToRequestBody a, FromResponse b) => Manager -> Request a -> IO (Response b)
sendWith manager req = do
  llreq <- toLowlevelRequest req
  -- Close the connection if building the body fails before anyone owns it.
  bracketOnError (LowLevelClient.responseOpen llreq manager) LowLevelClient.responseClose $ \llres -> do
    let status = LowLevelStatus.statusCode (LowLevelClient.responseStatus llres)
        headers = map (\(k, v) -> (CI.original k, v)) (LowLevelClient.responseHeaders llres)
        readChunk = do
          chunk <- LowLevelClient.brRead (LowLevelClient.responseBody llres)
          return $ if BS.null chunk then Nothing else Just chunk
        stream = StreamBody readChunk (LowLevelClient.responseClose llres)
    Response status headers <$> fromResponse (Response status headers stream)

send :: (ToRequestBody a, FromResponse b) => Request a -> IO (Response b)
send req = do
  manager <- LowLevelTLSClient.getGlobalManager
  sendWith manager req

get :: (FromResponse a) => String -> IO (Response a)
get url = send $ Request GET url [] ()

delete :: (FromResponse a) => String -> IO (Response a)
delete url = send $ Request DELETE url [] ()

post :: (ToRequestBody a, FromResponse b) => String -> a -> IO (Response b)
post url body = send $ Request POST url [] body

put :: (ToRequestBody a, FromResponse b) => String -> a -> IO (Response b)
put url body = send $ Request PUT url [] body

patch :: (ToRequestBody a, FromResponse b) => String -> a -> IO (Response b)
patch url body = send $ Request PATCH url [] body
