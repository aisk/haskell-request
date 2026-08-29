module Network.HTTP.Request.Internal.Status
  ( StatusException (..),
    raiseForStatus,
  )
where

import Control.Exception (Exception, throwIO)
import Network.HTTP.Request.Internal.Types
  ( Headers,
    Response,
    responseHeaders,
    responseStatus,
  )

data StatusException = StatusException Int Headers
  deriving (Show)

instance Exception StatusException

raiseForStatus :: Response a -> IO (Response a)
raiseForStatus res
  | responseStatus res >= 400 =
      throwIO $ StatusException (responseStatus res) (responseHeaders res)
  | otherwise = return res
