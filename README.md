# request

![](https://miro.medium.com/max/1200/1*5KglaZoNp4fNpNHUao5u5w.jpeg)

HTTP client for haskell, inpired by [requests](https://requests.readthedocs.io/) and [http-dispatch](https://github.com/owainlewis/http-dispatch).

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/aisk/request)

## Installation

This pacakge is published on [hackage](http://hackage.haskell.org/package/request) with the same name `request`, you can install it with cabal or stack or nix as any other hackage packages.

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
resp <- get "https://api.leancloud.cn/1.1/date"
print resp.status        -- 200

-- Or construct a Request manually
let req = Request { method = GET, url = "https://api.leancloud.cn/1.1/date", headers = [], body = (Nothing :: Maybe BS.ByteString) }

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
  , body    :: Maybe a
  } deriving (Show)
```

Built-in `ToRequestBody` instances and their inferred `Content-Type`:

- `ByteString` / lazy `ByteString` / `Text` / `String` → `text/plain; charset=utf-8`
- Any type with a `ToJSON` instance → auto JSON encoding + `application/json`

The `Content-Type` is automatically inferred from the body type. You can override it by setting the header manually:

```haskell
-- Content-Type is auto-inferred from body type
send $ Request POST url [] (Just body)

-- Or override Content-Type manually
send $ Request POST url [("Content-Type", "text/xml")] (Just xmlBytes)
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

The response body type `a` can be any type that implements the `FromResponseBody` constraint, allowing flexible handling of response data. Built-in supported types include `String`, `ByteString`, `Text`, and any type with a `FromJSON` instance.

### send

Once you have constructed your own `Request` record, you can call the `send` function to send it to the server. It automatically serializes the body and infers the `Content-Type` header. The `send` function's type is:

```haskell
send :: (ToRequestBody a, FromResponseBody b) => Request a -> IO (Response b)
```

## JSON Support

### JSON Response

For any type with a `FromJSON` instance, the response body will be automatically decoded:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Network.HTTP.Request
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data Date = Date
  { __type :: String
  , iso :: String
  } deriving (Show, Generic)

instance FromJSON Date

main :: IO ()
main = do
  response <- get "https://api.leancloud.cn/1.1/date" :: IO (Response Date)
  print response.status  -- 200
  print response.body    -- Date { __type = "Date", iso = "..." }
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

## Shortcuts

As you expected, there are some shortcuts for the most used scenarios.

```haskell
get    :: (FromResponseBody a) => String -> IO (Response a)
delete :: (FromResponseBody a) => String -> IO (Response a)
post   :: (ToRequestBody a, FromResponseBody b) => String -> a -> IO (Response b)
put    :: (ToRequestBody a, FromResponseBody b) => String -> a -> IO (Response b)
patch  :: (ToRequestBody a, FromResponseBody b) => String -> a -> IO (Response b)
```

These shortcuts' definitions are simple and direct. You are encouraged to add your own if the built-in does not match your use cases, like add custom headers in every request.

## Without Language Extensions

If you prefer not to use the language extensions, you can still use the library with the traditional syntax:

- Create requests using positional arguments: `Request GET "url" [] (Nothing :: Maybe BS.ByteString)`
- Use prefixed accessor functions: `responseStatus response`, `responseHeaders response`, etc.

```haskell
import Network.HTTP.Request
import qualified Data.ByteString as BS

-- Construct a Request using positional arguments
let req = Request GET "https://api.leancloud.cn/1.1/date" [] (Nothing :: Maybe BS.ByteString)
-- Send it
res <- send req
-- Access the fields using prefixed accessor functions
print $ responseStatus res
```

## Streaming Support

For large responses or real-time data, you can stream the response body instead of buffering it all in memory.

### Raw Byte Chunks

Use `StreamBody BS.ByteString` to receive the response body as a stream of raw byte chunks:

```haskell
import Network.HTTP.Request
import qualified Data.ByteString as BS

main :: IO ()
main = do
  let req = Request GET "https://example.com/large-file" [] (Nothing :: Maybe BS.ByteString)
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
  let req = Request GET "https://example.com/events" [] (Nothing :: Maybe BS.ByteString)
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

## API Documents

See the hackage page: http://hackage.haskell.org/package/request/docs/Network-HTTP-Request.html

## About the Project

Request is &copy; 2020-2026 by [AN Long](https://github.com/aisk).

### License

Request is distributed by a [BSD license](https://github.com/aisk/request/tree/master/LICENSE).
