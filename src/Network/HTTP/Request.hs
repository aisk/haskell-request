{-# LANGUAGE DuplicateRecordFields #-}

module Network.HTTP.Request
  ( Header,
    Headers,
    FromResponseBody (..),
    ToRequestBody (..),
    ToForm (..),
    Form (..),
    Manager,
    Method (..),
    Request (..),
    Response (..),
    ResponseBodyException (..),
    StatusException (..),
    StreamBody (..),
    SseEvent (..),
    basicAuth,
    get,
    delete,
    patch,
    post,
    put,
    newManager,
    raiseForStatus,
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

import Network.HTTP.Request.Internal.Auth (basicAuth)
import Network.HTTP.Request.Internal.Body
  ( Form (..),
    FromResponseBody (..),
    ResponseBodyException (..),
    ToForm (..),
    ToRequestBody (..),
  )
import Network.HTTP.Request.Internal.Client
  ( Manager,
    delete,
    get,
    newManager,
    patch,
    post,
    put,
    send,
    sendWith,
  )
import Network.HTTP.Request.Internal.Status
  ( StatusException (..),
    raiseForStatus,
  )
import Network.HTTP.Request.Internal.Types
  ( Header,
    Headers,
    Method (..),
    Request (..),
    Response (..),
    SseEvent (..),
    StreamBody (..),
    requestBody,
    requestHeaders,
    requestMethod,
    requestUrl,
    responseBody,
    responseHeaders,
    responseStatus,
  )
