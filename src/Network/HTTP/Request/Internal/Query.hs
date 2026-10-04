module Network.HTTP.Request.Internal.Query
  ( addQuery,
  )
where

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.URI (renderSimpleQuery)

-- | Append query parameters to a URL. Keys and values are UTF-8 encoded and
-- percent-escaped. Parameters already in the URL are kept, and a fragment
-- stays at the end.
addQuery :: String -> [(T.Text, T.Text)] -> String
addQuery url [] = url
addQuery url params = base ++ separator ++ C.unpack rendered ++ fragment
  where
    (base, fragment) = break (== '#') url
    rendered = renderSimpleQuery False [(T.encodeUtf8 k, T.encodeUtf8 v) | (k, v) <- params]
    separator
      | '?' `notElem` base = "?"
      | last base `elem` "?&" = ""
      | otherwise = "&"
