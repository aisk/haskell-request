{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (isInfixOf)
import GHC.Generics (Generic)
import Data.List (mapAccumL)
import Network.HTTP.Request
import Network.HTTP.Request.Internal.Sse (feedSse, newSseParser)
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data UUID = UUID
  { uuid :: String
  } deriving (Show, Generic)

instance FromJSON UUID

data Greeting = Greeting
  { message :: String
  } deriving (Show, Generic)

instance ToJSON Greeting

data Login = Login
  { username :: T.Text
  , password :: T.Text
  } deriving (Show)

instance ToForm Login where
  toForm l = [ ("username", T.encodeUtf8 l.username)
             , ("password", T.encodeUtf8 l.password)
             ]

parseSse :: [BS.ByteString] -> [SseEvent]
parseSse = concat . snd . mapAccumL feedSse newSseParser

main :: IO ()
main = hspec $ do
  describe "SSE parser" $ do
    it "should parse events with data, event and id fields" $ do
      parseSse ["event: greet\nid: 1\ndata: hello\n\ndata: world\n\n"]
        `shouldBe` [SseEvent "hello" (Just "greet") (Just "1"), SseEvent "world" Nothing Nothing]

    it "should join multiple data lines and skip comments" $ do
      parseSse [": ping\ndata: line1\ndata:line2\n\n"]
        `shouldBe` [SseEvent "line1\nline2" Nothing Nothing]

    it "should accept CR, LF and CRLF line endings" $ do
      parseSse ["data: a\r\n\r\ndata: b\r\rdata: c\n\n"]
        `shouldBe` [SseEvent "a" Nothing Nothing, SseEvent "b" Nothing Nothing, SseEvent "c" Nothing Nothing]

    it "should parse events split across chunks" $ do
      parseSse ["da", "ta: he", "llo\r", "\n\r", "\ndata: wor", "ld\n", "\n"]
        `shouldBe` [SseEvent "hello" Nothing Nothing, SseEvent "world" Nothing Nothing]

    it "should strip a leading UTF-8 BOM" $ do
      parseSse ["\xEF\xBB\xBF" <> "data: hi\n\n"] `shouldBe` [SseEvent "hi" Nothing Nothing]
      parseSse ["\xEF\xBB", "\xBF" <> "data: hi\n\n"] `shouldBe` [SseEvent "hi" Nothing Nothing]

    it "should discard an incomplete event at the end of the stream" $ do
      parseSse ["data: done\n\ndata: partial\n"] `shouldBe` [SseEvent "done" Nothing Nothing]


  describe "Network.HTTP.Request" $ do
    let defaultUserAgent = "haskell-request/" <> VERSION_request

    it "should fetch example.com and return 200 OK" $ do
      response <- get "http://example.com" :: IO (Response String)
      responseStatus response `shouldBe` 200

    it "should send a request to example.com and return 200 OK" $ do
      response <- send (Request GET "http://example.com" [] ()) :: IO (Response String)
      responseStatus response `shouldBe` 200

    it "should post to postman-echo.com/post and return 200 OK" $ do
      response <- post "https://postman-echo.com/post" ("Hello!" :: BS.ByteString) :: IO (Response String)
      responseStatus response `shouldBe` 200

    it "should put to postman-echo.com/put and return 200 OK" $ do
      response <- put "https://postman-echo.com/put" ("Hello!" :: BS.ByteString) :: IO (Response String)
      responseStatus response `shouldBe` 200

    it "should patch to postman-echo.com/patch and return 200 OK" $ do
      response <- patch "https://postman-echo.com/patch" ("Hello!" :: BS.ByteString) :: IO (Response String)
      responseStatus response `shouldBe` 200

    it "should use dot record syntax to create and access request/response" $ do
      let req = Request { method = GET, url = "http://example.com", headers = [("User-Agent", "Haskell-Request")], body = () }
      response <- send req :: IO (Response String)
      response.status `shouldBe` 200
      response.headers `shouldSatisfy` (not . null)

    it "should access response body with different types" $ do
      -- Test with ByteString body
      let req1 = Request { method = GET, url = "http://example.com", headers = [], body = () }
      response1 <- send req1 :: IO (Response BS.ByteString)
      BS.length response1.body `shouldSatisfy` (> 0)

      -- Test with String body
      let req2 = Request { method = GET, url = "http://example.com", headers = [], body = () }
      response2 <- send req2 :: IO (Response String)
      not (null response2.body) `shouldBe` True

    it "should correctly handle UTF-8 encoded response (Chinese characters)" $ do
      let msg = "{\"message\":\"你好世界\"}"
      let req = Request
            { method = POST
            , url = "https://postman-echo.com/post"
            , headers = [("Content-Type", "application/json; charset=utf-8")]
            , body = T.encodeUtf8 $ T.pack msg
            }
      response <- send req
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf "你好世界"

    it "should correctly handle UTF-8 encoded response (emoji)" $ do
      let msg = "{\"message\":\"Hello 🌍\"}"
      let req = Request
            { method = POST
            , url = "https://postman-echo.com/post"
            , headers = [("Content-Type", "application/json; charset=utf-8")]
            , body = T.encodeUtf8 $ T.pack msg
            }
      response <- send req
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf "🌍"

    it "should parse JSON response with aeson" $ do
      response <- get "https://httpbin.org/uuid" :: IO (Response UUID)
      responseStatus response `shouldBe` 200
      uuid (responseBody response) `shouldSatisfy` not . null

    it "should post JSON body with automatic Content-Type" $ do
      response <- post "https://postman-echo.com/post" (Greeting "Hello!") :: IO (Response String)
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf "application/json"

    it "should post ByteString body as application/octet-stream" $ do
      response <- post "https://postman-echo.com/post" ("Hello!" :: BS.ByteString) :: IO (Response String)
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf "application/octet-stream"

    it "should post url-encoded form from a list" $ do
      response <- post "https://postman-echo.com/post" (Form [("foo", "bar"), ("baz", "qux")]) :: IO (Response String)
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf "application/x-www-form-urlencoded"
      responseBody response `shouldSatisfy` isInfixOf "\"foo\":\"bar\""
      responseBody response `shouldSatisfy` isInfixOf "\"baz\":\"qux\""

    it "should post url-encoded form from a ToForm instance" $ do
      response <- post "https://postman-echo.com/post" (Form (Login "alice" "s3cret")) :: IO (Response String)
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf "application/x-www-form-urlencoded"
      responseBody response `shouldSatisfy` isInfixOf "\"username\":\"alice\""
      responseBody response `shouldSatisfy` isInfixOf "\"password\":\"s3cret\""

    it "should percent-encode form values with special characters" $ do
      response <- post "https://postman-echo.com/post" (Form [("q", "hello world"), ("lang", "zh-CN")]) :: IO (Response String)
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf "\"q\":\"hello world\""

    it "should add default User-Agent when request header is missing" $ do
      response <- send (Request GET "https://postman-echo.com/get" [] ()) :: IO (Response String)
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf defaultUserAgent

    it "should not override user provided User-Agent" $ do
      let customUserAgent = "custom-user-agent-for-test"
      let req = Request GET "https://postman-echo.com/get" [("User-Agent", T.encodeUtf8 $ T.pack customUserAgent)] ()
      response <- send req :: IO (Response String)
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf customUserAgent
      responseBody response `shouldSatisfy` not . isInfixOf defaultUserAgent

    it "should build a Basic Authorization header value" $ do
      basicAuth "alice" "s3cret" `shouldBe` "Basic YWxpY2U6czNjcmV0"

    it "should send credentials from the URL as Basic Authorization" $ do
      response <- get "https://alice:s3cret@postman-echo.com/get" :: IO (Response String)
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf "Basic YWxpY2U6czNjcmV0"

    it "should prefer a user provided Authorization header over URL credentials" $ do
      let req = Request GET "https://alice:s3cret@postman-echo.com/get" [("authorization", "Bearer my-token")] ()
      response <- send req :: IO (Response String)
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf "Bearer my-token"
      responseBody response `shouldSatisfy` not . isInfixOf "Basic YWxpY2U6czNjcmV0"

    it "should ignore the response body when the response type is unit" $ do
      response <- get "http://example.com" :: IO (Response ())
      responseStatus response `shouldBe` 200
      responseBody response `shouldBe` ()

    it "should return the response unchanged when raiseForStatus sees a 2xx status" $ do
      let resp = Response { status = 200, headers = [("X-Test", "1")], body = "ok" :: String }
      checked <- raiseForStatus resp
      checked.status `shouldBe` 200
      checked.body `shouldBe` "ok"

    it "should throw StatusException from raiseForStatus on a 4xx status" $ do
      let resp = Response { status = 404, headers = [("X-Test", "1")], body = () }
      raiseForStatus resp `shouldThrow` \(StatusException code hdrs) ->
        code == 404 && hdrs == [("X-Test", "1")]

    it "should throw StatusException for an error status from a real server" $ do
      let sendAndRaise = do
            resp <- get "https://postman-echo.com/status/500" :: IO (Response String)
            raiseForStatus resp
      sendAndRaise `shouldThrow` \(StatusException code _) -> code == 500

    it "should throw HttpException when the connection fails" $ do
      (get "http://localhost:1" :: IO (Response String)) `shouldThrow` \case
        HttpExceptionRequest _ (ConnectionFailure _) -> True
        _ -> False

    it "should throw HttpException for an invalid URL" $ do
      (get "not a url" :: IO (Response String)) `shouldThrow` \case
        InvalidUrlException _ _ -> True
        _ -> False

    it "should send with a user-provided manager" $ do
      mgr <- newManager
      response <- sendWith mgr (Request GET "http://example.com" [] ()) :: IO (Response String)
      responseStatus response `shouldBe` 200

    it "should stream response body as raw byte chunks" $ do
      let req = Request GET "http://example.com" [] ()
      resp <- send req :: IO (Response (StreamBody BS.ByteString))
      resp.status `shouldBe` 200
      mChunk <- resp.body.readNext
      resp.body.closeStream
      mChunk `shouldSatisfy` (/= Nothing)

    it "should parse and stream SSE events" $ do
      let req = Request GET "https://stream.wikimedia.org/v2/stream/recentchange" [] ()
      resp <- send req :: IO (Response (StreamBody SseEvent))
      resp.status `shouldBe` 200
      mEvent <- resp.body.readNext
      resp.body.closeStream
      case mEvent of
        Nothing -> expectationFailure "Expected at least one SSE event from the stream"
        Just _ -> return ()
