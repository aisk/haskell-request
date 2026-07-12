{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Request.Internal.Sse
  ( findEventSep,
    parseSseBlock,
  )
where

import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Request.Internal.Types (SseEvent (..))

findEventSep :: BS.ByteString -> Maybe (Int, Int)
findEventSep bs =
  foldr
    earlier
    Nothing
    [ tryFind "\r\n\r\n",
      tryFind "\r\n\n",
      tryFind "\r\n\r",
      tryFind "\n\r\n",
      tryFind "\n\n",
      tryFind "\n\r",
      tryFind "\r\r\n",
      tryFind "\r\r"
    ]
  where
    tryFind pat =
      let (h, t) = BS.breakSubstring pat bs
       in if BS.null t then Nothing else Just (BS.length h, BS.length h + BS.length pat)
    earlier a Nothing = a
    earlier Nothing b = b
    earlier a@(Just (s1, e1)) b@(Just (s2, e2))
      | s1 < s2 = a
      | s1 > s2 = b
      | e1 >= e2 = a
      | otherwise = b

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

parseSseBlock :: BS.ByteString -> Maybe SseEvent
parseSseBlock block =
  let txt = T.replace "\r" "\n" . T.replace "\r\n" "\n" $ T.decodeUtf8Lenient block
      ls = T.lines txt
      fields = mapMaybe parseSseField ls
      dataFields = [v | (k, v) <- fields, k == "data"]
      dataVal = T.intercalate "\n" dataFields
      lastField name = foldl' (\current (k, v) -> if k == name then Just v else current) Nothing fields
      typeVal = lastField "event"
      idVal = lastField "id"
   in if null dataFields then Nothing else Just (SseEvent dataVal typeVal idVal)
