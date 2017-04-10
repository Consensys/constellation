{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Wai where

import ClassyPrelude

import Network.HTTP.Types ( ok200, badRequest400, unauthorized401, notFound404
                          , internalServerError500
                          )
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wai as Wai

ok :: BL.ByteString -> Wai.Response
ok = Wai.responseLBS ok200 []

ok' :: ByteString -> Wai.Response
ok' = ok . BL.fromStrict

badRequest :: Wai.Response
badRequest = Wai.responseLBS badRequest400 [] "Bad Request"

unauthorized :: Wai.Response
unauthorized = Wai.responseLBS unauthorized401 [] "Unauthorized"

notFound :: Wai.Response
notFound = Wai.responseLBS notFound404 [] "Not Found"

internalServerError :: Wai.Response
internalServerError = Wai.responseLBS internalServerError500 [] "Internal Server Error"
