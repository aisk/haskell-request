{-# LANGUAGE DuplicateRecordFields #-}

module Network.HTTP.Request.Internal.Types
  ( Header,
    Headers,
    Method (..),
    Request (..),
    Response (..),
    StreamBody (..),
    SseEvent (..),
    requestMethod,
    requestUrl,
    requestHeaders,
    requestBody,
    responseStatus,
    responseHeaders,
    responseBody,
  )
where

import qualified Data.ByteString as BS
import qualified Data.Text as T

type Header = (BS.ByteString, BS.ByteString)

type Headers = [Header]

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
    body :: a
  }
  deriving (Show)

data Response a = Response
  { status :: Int,
    headers :: Headers,
    body :: a
  }
  deriving (Show)

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

requestMethod :: Request a -> Method
requestMethod (Request value _ _ _) = value

requestUrl :: Request a -> String
requestUrl (Request _ value _ _) = value

requestHeaders :: Request a -> Headers
requestHeaders (Request _ _ value _) = value

requestBody :: Request a -> a
requestBody (Request _ _ _ value) = value

responseStatus :: Response a -> Int
responseStatus (Response value _ _) = value

responseHeaders :: Response a -> Headers
responseHeaders (Response _ value _) = value

responseBody :: Response a -> a
responseBody (Response _ _ value) = value
