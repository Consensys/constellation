{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Http where

import ClassyPrelude
import Network.HTTP.Conduit
    ( Manager, Request, RequestBody(..), Response
    , parseRequest, httpLbs, method, secure, requestBody
    )
import Network.HTTP.Types (HeaderName, RequestHeaders)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

getHeaderCommaValues :: HeaderName -> RequestHeaders -> [ByteString]
getHeaderCommaValues hname h =
    concatMap (BC.split ',') (getHeaderValues hname h)

getHeaderValues :: HeaderName -> RequestHeaders -> [ByteString]
getHeaderValues hname h = [v | (k, v) <- h, k == hname]

postRequestBs :: MonadThrow m => String -> ByteString -> m Request
postRequestBs url b = postRequest url (RequestBodyBS b)

postRequestLbs :: MonadThrow m => String -> BL.ByteString -> m Request
postRequestLbs url b = postRequest url (RequestBodyLBS b)

postRequest :: MonadThrow m => String -> RequestBody -> m Request
postRequest url b = parseRequest url >>= \req -> return req
    { method      = "POST"
    , secure      = True  -- TODO: This needs to be toggled
    , requestBody = b
    }

simplePostBs :: (MonadThrow m, MonadIO m)
             => Manager
             -> String
             -> ByteString
             -> m (Response BL.ByteString)
simplePostBs m url b = postRequestBs url b >>= \req -> httpLbs req m

simplePostLbs :: (MonadThrow m, MonadIO m)
              => Manager
              -> String
              -> BL.ByteString
              -> m (Response BL.ByteString)
simplePostLbs m url b = postRequestLbs url b >>= \req -> httpLbs req m
