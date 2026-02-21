{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (isInfixOf)
import GHC.Generics (Generic)
import Network.HTTP.Request
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Date = Date
  { __type :: String
  , iso :: String
  } deriving (Show, Generic)

instance FromJSON Date

data Greeting = Greeting
  { message :: String
  } deriving (Show, Generic)

instance ToJSON Greeting

main :: IO ()
main = hspec $ do
  describe "Network.HTTP.Request" $ do
    it "should fetch example.com and return 200 OK" $ do
      response <- get "http://example.com" :: IO (Response String)
      responseStatus response `shouldBe` 200

    it "should send a request to example.com and return 200 OK" $ do
      response <- send (Request GET "http://example.com" [] (Nothing :: Maybe BS.ByteString)) :: IO (Response String)
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
      let req = Request { method = GET, url = "http://example.com", headers = [("User-Agent", "Haskell-Request")], body = (Nothing :: Maybe BS.ByteString) }
      response <- send req :: IO (Response String)
      response.status `shouldBe` 200
      response.headers `shouldSatisfy` (not . null)

    it "should access response body with different types" $ do
      -- Test with ByteString body
      let req1 = Request { method = GET, url = "http://example.com", headers = [], body = (Nothing :: Maybe BS.ByteString) }
      response1 <- send req1 :: IO (Response BS.ByteString)
      BS.length response1.body `shouldSatisfy` (> 0)

      -- Test with String body
      let req2 = Request { method = GET, url = "http://example.com", headers = [], body = (Nothing :: Maybe BS.ByteString) }
      response2 <- send req2 :: IO (Response String)
      not (null response2.body) `shouldBe` True

    it "should correctly handle UTF-8 encoded response (Chinese characters)" $ do
      let msg = "{\"message\":\"你好世界\"}"
      let req = Request
            { method = POST
            , url = "https://postman-echo.com/post"
            , headers = [("Content-Type", "application/json; charset=utf-8")]
            , body = Just (T.encodeUtf8 $ T.pack msg)
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
            , body = Just (T.encodeUtf8 $ T.pack msg)
            }
      response <- send req
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf "🌍"

    it "should parse JSON response with aeson" $ do
      response <- get "https://api.leancloud.cn/1.1/date" :: IO (Response Date)
      responseStatus response `shouldBe` 200
      __type (responseBody response) `shouldBe` "Date"
      iso (responseBody response) `shouldSatisfy` not . null

    it "should post JSON body with automatic Content-Type" $ do
      response <- post "https://postman-echo.com/post" (Greeting "Hello!") :: IO (Response String)
      responseStatus response `shouldBe` 200
      responseBody response `shouldSatisfy` isInfixOf "application/json"

    it "should stream response body as raw byte chunks" $ do
      let req = Request GET "http://example.com" [] (Nothing :: Maybe BS.ByteString)
      resp <- send req :: IO (Response (StreamBody BS.ByteString))
      resp.status `shouldBe` 200
      mChunk <- resp.body.readNext
      resp.body.closeStream
      mChunk `shouldSatisfy` (/= Nothing)

    it "should parse and stream SSE events" $ do
      let req = Request GET "https://sse.dev/test" [] (Nothing :: Maybe BS.ByteString)
      resp <- send req :: IO (Response (StreamBody SseEvent))
      resp.status `shouldBe` 200
      mEvent <- resp.body.readNext
      resp.body.closeStream
      case mEvent of
        Nothing -> expectationFailure "Expected at least one SSE event from the stream"
        Just _ -> return ()
