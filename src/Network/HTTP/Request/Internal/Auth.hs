{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Request.Internal.Auth
  ( basicAuth,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64

basicAuth :: BS.ByteString -> BS.ByteString -> BS.ByteString
basicAuth username password = "Basic " <> Base64.encode (username <> ":" <> password)
