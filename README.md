# request

![](https://miro.medium.com/max/1200/1*5KglaZoNp4fNpNHUao5u5w.jpeg)

HTTP client for haskell, inspired by [requests](https://requests.readthedocs.io/) and [http-dispatch](https://github.com/owainlewis/http-dispatch).

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/aisk/haskell-request)

## Installation

This package is published on [hackage](http://hackage.haskell.org/package/request) with the same name `request`, you can install it with cabal or stack or nix as any other hackage packages.

## Usage

This library supports modern Haskell record dot syntax. First, enable these language extensions:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
```

Then you can use the library like this:

```haskell
import Network.HTTP.Request
import qualified Data.ByteString as BS

-- Using shortcuts
resp <- get "https://httpbin.org/uuid" :: IO (Response String)
print resp.status        -- 200

-- Or construct a Request manually
let req = Request { method = GET, url = "https://httpbin.org/uuid", headers = [], body = () }

-- Response with ByteString body
responseBS <- send req :: IO (Response BS.ByteString)
print responseBS.status        -- 200
print responseBS.body          -- ByteString response

-- Response with String body
responseStr <- send req :: IO (Response String)
print responseStr.body         -- String response
```

## Core API

Request's API has three core concepts: `Request` record type, `Response` record type, `send` function.

### Request

`Request a` is all about the information you will send to the target URL. The type parameter `a` is the body type, it can be any type that implements `ToRequestBody`. When `send` is called, the body is automatically serialized and the appropriate `Content-Type` header is inferred, unless you set it manually.

```haskell
data Request a = Request
  { method  :: Method
  , url     :: String
  , headers :: Headers
  , body    :: a
  } deriving (Show)
```

Built-in `ToRequestBody` instances and their inferred `Content-Type`:

- `()` → empty body, no Content-Type
- `ByteString` / lazy `ByteString` → `application/octet-stream`
- `Text` / `String` → `text/plain; charset=utf-8`
- Any type with a `ToJSON` instance → auto JSON encoding + `application/json`
- `Form a` (where `a` has a `ToForm` instance) → URL-encoded + `application/x-www-form-urlencoded`

The `Content-Type` is automatically inferred from the body type. You can override it by setting the header manually:

```haskell
-- Content-Type is auto-inferred from body type
send $ Request POST url [] body

-- Or override Content-Type manually
send $ Request POST url [("Content-Type", "text/xml")] xmlBytes
```

### Response

`Response` is what you got from the server URL.

```haskell
data Response a = Response
  { status  :: Int
  , headers :: Headers
  , body    :: a
  } deriving (Show)
```

The response body type `a` can be any type that implements the `FromResponse` constraint, allowing flexible handling of response data. Built-in supported types include `String`, `ByteString`, `Text`, and any type with a `FromJSON` instance.

`String` and `Text` bodies are decoded with the charset declared in the response's `Content-Type` header, so a `text/html; charset=GBK` page comes back as proper text. Invalid bytes are replaced with U+FFFD. When the charset is missing or not known to the system, the body is decoded as UTF-8.

### send

Once you have constructed your own `Request` record, you can call the `send` function to send it to the server. It automatically serializes the body and infers the `Content-Type` header. The `send` function's type is:

```haskell
send :: (ToRequestBody a, FromResponse b) => Request a -> IO (Response b)
```

## JSON Support

### JSON Response

For any type with a `FromJSON` instance, the response body will be automatically decoded:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Network.HTTP.Request
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data UUID = UUID
  { uuid :: String
  } deriving (Show, Generic)

instance FromJSON UUID

main :: IO ()
main = do
  response <- get "https://httpbin.org/uuid" :: IO (Response UUID)
  print response.status  -- 200
  print response.body    -- UUID { uuid = "550e8400-e29b-41d4-a716-446655440000" }
```

If JSON decoding fails, an `AesonException` will be thrown, which can be caught with `Control.Exception.catch` or `try`.

### JSON Request Body

The `post`, `put`, and `patch` shortcuts accept any type that implements `ToRequestBody`. For types with a `ToJSON` instance, the body is automatically JSON-encoded and `Content-Type: application/json` is set:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Network.HTTP.Request
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data User = User { name :: String } deriving (Show, Generic)

instance ToJSON User

main :: IO ()
main = do
  response <- post "https://httpbin.org/post" (User "Alice") :: IO (Response String)
  print response.status  -- 200
```

## Form Support

For `application/x-www-form-urlencoded` requests (login forms, OAuth token endpoints, classic web APIs), wrap your body in the `Form` newtype. The `Content-Type` is set automatically and values are percent-encoded.

### From a list of pairs

```haskell
import Network.HTTP.Request

main :: IO ()
main = do
  response <- post "https://httpbin.org/post"
                   (Form [("username", "alice"), ("password", "s3cret")])
              :: IO (Response String)
  print response.status  -- 200
  -- Body sent: username=alice&password=s3cret
```

Values are `ByteString` keys and values. Special characters (spaces, Unicode, reserved chars) are percent-encoded for you:

```haskell
post "https://api.example.com/search"
     (Form [("q", "hello world"), ("lang", "zh-CN")])
-- Body sent: q=hello%20world&lang=zh-CN
```

### From a custom type

For your own record types, define a `ToForm` instance. This mirrors the `ToJSON` pattern:

```haskell
import Network.HTTP.Request
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Login = Login
  { username :: T.Text
  , password :: T.Text
  }

instance ToForm Login where
  toForm l = [ ("username", T.encodeUtf8 l.username)
             , ("password", T.encodeUtf8 l.password)
             ]

main :: IO ()
main = do
  response <- post "https://api.example.com/login"
                   (Form (Login "alice" "s3cret"))
              :: IO (Response String)
  print response.status
```

### The two new pieces of API

```haskell
class ToForm a where
  toForm :: a -> [(ByteString, ByteString)]

newtype Form a = Form a
```

The `Form` newtype is required to disambiguate the form-encoding path from JSON. Without it, a type that has both `ToJSON` and `ToForm` instances would be ambiguous; with it, `post url x` always means JSON and `post url (Form x)` always means form.

## Shortcuts

As you expected, there are some shortcuts for the most used scenarios.

```haskell
get    :: (FromResponse a) => String -> IO (Response a)
delete :: (FromResponse a) => String -> IO (Response a)
post   :: (ToRequestBody a, FromResponse b) => String -> a -> IO (Response b)
put    :: (ToRequestBody a, FromResponse b) => String -> a -> IO (Response b)
patch  :: (ToRequestBody a, FromResponse b) => String -> a -> IO (Response b)
```

These shortcuts' definitions are simple and direct. You are encouraged to add your own if the built-in does not match your use cases, like add custom headers in every request.

## Query Parameters

`addQuery` appends query parameters to a URL and takes care of the escaping:

```haskell
let url = "https://api.example.com/search" `addQuery` [("q", "haskell request"), ("page", "2")]
response <- get url :: IO (Response String)
-- GET https://api.example.com/search?q=haskell%20request&page=2
```

It is a plain `String -> [(Text, Text)] -> String` function, so it works with `Request` and every shortcut. Parameters already in the URL are kept.

## Authentication

`basicAuth` builds the value of a Basic `Authorization` header. Put it in the request's header list yourself:

```haskell
let req = Request GET url [("Authorization", basicAuth "username" "password")] ()
response <- send req :: IO (Response String)
```

## Checking Response Status

A response with a 4xx or 5xx status is returned as-is. If you prefer to treat error statuses as exceptions, like `raise_for_status` in Python requests, pass the response through `raiseForStatus`:

```haskell
resp <- get "https://httpbin.org/status/404" >>= raiseForStatus :: IO (Response String)
-- throws: StatusException 404 [("Content-Type", ...), ...]
```

`raiseForStatus` returns the response unchanged when the status is below 400, and throws a `StatusException` carrying the status code and the response headers otherwise:

```haskell
data StatusException = StatusException Int Headers

raiseForStatus :: Response a -> IO (Response a)
```

## Network Errors

Connection failures, timeouts and invalid URLs are reported as `http-client`'s `HttpException`. It is re-exported together with `HttpExceptionContent`, so you can catch it without depending on `http-client` yourself:

```haskell
import Control.Exception (try)
import Network.HTTP.Request

main :: IO ()
main = do
  result <- try (get "https://example.invalid") :: IO (Either HttpException (Response String))
  case result of
    Left (HttpExceptionRequest _ content) -> print content   -- e.g. ConnectionFailure ...
    Left (InvalidUrlException url reason) -> putStrLn (url <> ": " <> reason)
    Right resp -> print resp.status
```

## Without Language Extensions

If you prefer not to use the language extensions, you can still use the library with the traditional syntax:

- Create requests using positional arguments: `Request GET "url" [] ()`
- Use prefixed accessor functions: `responseStatus response`, `responseHeaders response`, etc.

```haskell
import Network.HTTP.Request

-- Construct a Request using positional arguments
let req = Request GET "https://httpbin.org/uuid" [] ()
-- Send it
res <- send req :: IO (Response String)
-- Access the fields using prefixed accessor functions
print $ responseStatus res
```

## Custom Connection Manager

By default, `send` uses the `http-client` global TLS manager. For most applications this is fine, you get connection pooling for free with no setup. If you want to isolate your library's connection pool from the rest of the program, keep a long-lived manager in a service, or configure proxies and custom TLS settings, create your own manager and pass it to `sendWith`:

```haskell
import Network.HTTP.Request

main :: IO ()
main = do
  mgr <- newManager
  resp <- sendWith mgr (Request GET "https://api.example.com/things" [] ()) :: IO (Response String)
  print resp.status
```

The two new pieces of API:

```haskell
newManager :: IO Manager
sendWith   :: (ToRequestBody a, FromResponse b) => Manager -> Request a -> IO (Response b)
```

`Manager` is the same type as `Network.HTTP.Client.Manager`, re-exported for convenience. For deeper configuration (`ManagerSettings`, custom proxies, certificate pinning, etc.) import `Network.HTTP.Client` / `Network.HTTP.Client.TLS` directly and build a `Manager` however you need. `sendWith` accepts it as-is.

### Timeouts

Requests time out after 30 seconds by default, which is the `http-client` default. The timeout covers connecting and waiting for the response headers, not reading the body, so long-lived streams are not cut off. To change it, build a manager with a different `managerResponseTimeout` (in microseconds). This needs `http-client` and `http-client-tls` in your `build-depends`:

```haskell
import Network.HTTP.Request
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as TLS

main :: IO ()
main = do
  mgr <- HC.newManager TLS.tlsManagerSettings
    { HC.managerResponseTimeout = HC.responseTimeoutMicro 5000000 }  -- 5 seconds
  resp <- sendWith mgr (Request GET "https://api.example.com/things" [] ()) :: IO (Response String)
  print resp.status
```

Use `HC.responseTimeoutNone` to disable the timeout. A timed out request throws `HttpExceptionRequest` with `ResponseTimeout` or `ConnectionTimeout`.

To apply the same setting to `send` and the shortcut functions, install the manager globally with `TLS.setGlobalManager mgr`.

## Streaming Support

For large responses or real-time data, you can stream the response body instead of buffering it all in memory.

### Raw Byte Chunks

Use `StreamBody BS.ByteString` to receive the response body as a stream of raw byte chunks:

```haskell
import Network.HTTP.Request
import qualified Data.ByteString as BS

main :: IO ()
main = do
  let req = Request GET "https://example.com/large-file" [] ()
  resp <- send req :: IO (Response (StreamBody BS.ByteString))
  print resp.status  -- 200

  let loop = do
        mChunk <- resp.body.readNext
        case mChunk of
          Nothing    -> return ()          -- stream finished
          Just chunk -> do
            BS.putStr chunk
            loop
  loop
  resp.body.closeStream
```

### SSE (Server-Sent Events)

Use `StreamBody SseEvent` to automatically parse an SSE stream. Each call to `readNext` returns the next complete event:

```haskell
import Network.HTTP.Request
import qualified Data.Text.IO as T

data SseEvent = SseEvent
  { sseData :: T.Text       -- content of the "data:" field
  , sseType :: Maybe T.Text -- content of the "event:" field
  , sseId   :: Maybe T.Text -- content of the "id:" field
  }

main :: IO ()
main = do
  let req = Request GET "https://example.com/events" [] ()
  resp <- send req :: IO (Response (StreamBody SseEvent))
  print resp.status  -- 200

  let loop = do
        mEvent <- resp.body.readNext
        case mEvent of
          Nothing    -> return ()          -- stream finished
          Just event -> do
            T.putStrLn event.sseData
            loop
  loop
  resp.body.closeStream
```

`StreamBody` has two fields:

- `readNext :: IO (Maybe a)` — reads the next chunk or event; returns `Nothing` when the stream ends
- `closeStream :: IO ()` — closes the underlying connection

## Custom Response Types

To support your own response body type, implement `FromResponse`. Its single method receives the response before the body has been read:

```haskell
class FromResponse a where
  fromResponse :: Response (StreamBody ByteString) -> IO a
```

Most instances just want the whole body. `decodeResponse` buffers it, closes the connection and runs a pure decoder that can also look at the status and headers. A `Left` is thrown as `ResponseBodyException`:

```haskell
import Network.HTTP.Request
import qualified Data.ByteString.Lazy.Char8 as LBS

newtype Lines = Lines [LBS.ByteString]

instance FromResponse Lines where
  fromResponse = decodeResponse $ \res ->
    if res.status < 400
      then Right (Lines (LBS.lines res.body))
      else Left ("unexpected status " <> show res.status)
```

The two helpers:

```haskell
bufferResponse :: Response (StreamBody ByteString) -> IO (Response LazyByteString)
decodeResponse :: (Response LazyByteString -> Either String a) -> Response (StreamBody ByteString) -> IO a
```

Use `bufferResponse` when you need IO or want to throw your own exception type. An instance that neither calls these helpers nor returns the stream to the caller must call `closeStream` itself.

## API Documents

See the hackage page: http://hackage.haskell.org/package/request/docs/Network-HTTP-Request.html

## About the Project

Request is &copy; 2020-2026 by [AN Long](https://github.com/aisk).

### License

Request is distributed by a [BSD license](https://github.com/aisk/haskell-request/tree/master/LICENSE).
