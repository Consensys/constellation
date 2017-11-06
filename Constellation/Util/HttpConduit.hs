{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.HttpConduit where

import ClassyPrelude
import Network.HTTP.Conduit
    ( Manager, Request, Response, HttpException(HttpExceptionRequest)
    , HttpExceptionContent(ResponseTimeout), httpLbs
    )
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E

httpLbsRetryTimeouts :: Request -> Manager -> Int -> IO (Response BL.ByteString)
httpLbsRetryTimeouts req m 0 = httpLbs req m
httpLbsRetryTimeouts req m n = E.catch (httpLbs req m) $
    \ex -> case ex of
        HttpExceptionRequest _ ResponseTimeout -> httpLbsRetryTimeouts req m $! n-1
        _                                      -> E.throwIO ex
