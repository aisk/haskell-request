{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Request.Internal.Sse
  ( SseParser,
    newSseParser,
    feedSse,
  )
where

import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Request.Internal.Types (SseEvent (..))

-- | Incremental SSE parser state. Feed it chunks with 'feedSse'. Whatever is
-- still buffered when the stream ends is an incomplete event and is dropped.
data SseParser = SseParser
  { -- | Whether the leading UTF-8 BOM has been looked for.
    bomChecked :: Bool,
    -- | Pieces of the current unterminated line, newest first.
    pendingLine :: [BS.ByteString],
    -- | The previous line ended with CR, so a directly following LF belongs to it.
    skipLf :: Bool,
    -- | Fields of the current event, newest first.
    pendingFields :: [(T.Text, T.Text)]
  }

newSseParser :: SseParser
newSseParser = SseParser False [] False []

-- | Feed one chunk and get the events it completed. Each chunk is scanned
-- only once, so the cost is linear in the size of the stream.
feedSse :: SseParser -> BS.ByteString -> (SseParser, [SseEvent])
feedSse parser chunk
  | bomChecked parser = go parser chunk []
  | BS.length buf < BS.length bom && buf `BS.isPrefixOf` bom = (parser {pendingLine = [buf]}, [])
  | otherwise = go parser {bomChecked = True, pendingLine = []} (fromMaybe buf (BS.stripPrefix bom buf)) []
  where
    bom = "\xEF\xBB\xBF"
    buf = BS.concat (reverse (chunk : pendingLine parser))

    go p bs acc
      | BS.null bs = (p, reverse acc)
      | skipLf p = go p {skipLf = False} (if BS.head bs == lf then BS.drop 1 bs else bs) acc
      | BS.null rest = (p {pendingLine = h : pendingLine p}, reverse acc)
      | BS.null line = go p' {pendingFields = []} (BS.drop 1 rest) (maybe acc (: acc) event)
      | otherwise = go p' {pendingFields = maybe id (:) (parseSseField line) (pendingFields p)} (BS.drop 1 rest) acc
      where
        (h, rest) = BS.break (\c -> c == cr || c == lf) bs
        line = BS.concat (reverse (h : pendingLine p))
        p' = p {pendingLine = [], skipLf = BS.head rest == cr}
        event = buildSseEvent (reverse (pendingFields p))

    cr = 13
    lf = 10

parseSseField :: BS.ByteString -> Maybe (T.Text, T.Text)
parseSseField raw
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
  where
    line = T.decodeUtf8Lenient raw

buildSseEvent :: [(T.Text, T.Text)] -> Maybe SseEvent
buildSseEvent fields =
  let dataFields = [v | (k, v) <- fields, k == "data"]
      dataVal = T.intercalate "\n" dataFields
      lastField name = foldl' (\current (k, v) -> if k == name then Just v else current) Nothing fields
      typeVal = lastField "event"
      idVal = lastField "id"
   in if null dataFields then Nothing else Just (SseEvent dataVal typeVal idVal)
