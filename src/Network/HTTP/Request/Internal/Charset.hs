{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.HTTP.Request.Internal.Charset
  ( charsetFromHeaders,
    decodeText,
  )
where

import Control.Exception (IOException, catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.CaseInsensitive as CI
import Data.Char (isAlphaNum, isAscii, toLower)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Foreign (peekCStringLen)
import GHC.IO.Encoding (mkTextEncoding)
import Network.HTTP.Request.Internal.Types (Headers)

-- | Extract the charset parameter of the Content-Type header, if any.
charsetFromHeaders :: Headers -> Maybe BS.ByteString
charsetFromHeaders headers = do
  contentType <- lookup "content-type" [(CI.mk k, v) | (k, v) <- headers]
  listToMaybe
    [ charset
      | param <- drop 1 (C.split ';' contentType),
        let (key, value) = C.break (== '=') param,
        CI.mk (C.strip key) == "charset",
        let charset = C.filter (/= '"') (C.strip (C.drop 1 value)),
        not (BS.null charset)
    ]

-- | Decode bytes with the given charset, replacing invalid input with U+FFFD.
-- A missing charset, or one the system does not know, is treated as UTF-8.
decodeText :: Maybe BS.ByteString -> BS.ByteString -> IO T.Text
decodeText charset bytes = case C.map toLower <$> charset of
  Nothing -> return utf8
  Just name
    | name `elem` ["utf-8", "utf8"] -> return utf8
    | name `elem` ["iso-8859-1", "latin1", "us-ascii", "ascii"] -> return (T.decodeLatin1 bytes)
    | C.all isNameChar name -> decodeWith name `catch` \(_ :: IOException) -> return utf8
    | otherwise -> return utf8
  where
    utf8 = T.decodeUtf8Lenient bytes
    isNameChar c = isAscii c && (isAlphaNum c || c `elem` ("-_.:+" :: String))
    -- Other charsets go through the encodings GHC knows about, which is iconv
    -- on POSIX systems and the code pages (e.g. CP936) on Windows.
    decodeWith name = do
      encoding <- mkTextEncoding (C.unpack name ++ "//TRANSLIT")
      T.pack <$> BS.useAsCStringLen bytes (peekCStringLen encoding)
