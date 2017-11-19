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

postRequestBs :: MonadThrow m => Bool -> String -> ByteString -> m Request
postRequestBs setSecure url b = postRequest setSecure url (RequestBodyBS b)

postRequestLbs :: MonadThrow m => Bool -> String -> BL.ByteString -> m Request
postRequestLbs setSecure url b = postRequest setSecure url (RequestBodyLBS b)

postRequest :: MonadThrow m => Bool -> String -> RequestBody -> m Request
postRequest setSecure url b = parseRequest url >>= \req -> return req
    { method      = "POST"
    , secure      = setSecure || secure req
    , requestBody = b
    }

simplePostBs :: (MonadThrow m, MonadIO m)
             => Manager
             -> Bool
             -> String
             -> ByteString
             -> m (Response BL.ByteString)
simplePostBs m setSecure url b = postRequestBs setSecure url b
    >>= \req -> httpLbs req m

simplePostLbs :: (MonadThrow m, MonadIO m)
              => Manager
              -> Bool
              -> String
              -> BL.ByteString
              -> m (Response BL.ByteString)
simplePostLbs m setSecure url b = postRequestLbs setSecure url b
    >>= \req -> httpLbs req m
