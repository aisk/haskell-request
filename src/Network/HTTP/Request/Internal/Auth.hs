{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Request.Internal.Auth
  ( basicAuth,
    bearerAuth,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Request.Internal.Types
  ( Request (..),
    requestBody,
    requestHeaders,
    requestMethod,
    requestUrl,
  )

basicAuth :: BS.ByteString -> BS.ByteString -> Request a -> Request a
basicAuth username password =
  setAuthorizationHeader ("Basic " <> Base64.encode (username <> ":" <> password))

bearerAuth :: BS.ByteString -> Request a -> Request a
bearerAuth token = setAuthorizationHeader ("Bearer " <> token)

setAuthorizationHeader :: BS.ByteString -> Request a -> Request a
setAuthorizationHeader value req =
  Request
    (requestMethod req)
    (requestUrl req)
    ( ("Authorization", value)
        : filter (\(name, _) -> CI.mk name /= CI.mk ("Authorization" :: BS.ByteString)) (requestHeaders req)
    )
    (requestBody req)
