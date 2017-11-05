{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Http where

import ClassyPrelude
import Network.HTTP.Types (HeaderName, RequestHeaders)
import qualified Data.ByteString.Char8 as BC

getHeaderCommaValues :: HeaderName -> RequestHeaders -> [ByteString]
getHeaderCommaValues hname h =
    concatMap (BC.split ',') (getHeaderValues hname h)

getHeaderValues :: HeaderName -> RequestHeaders -> [ByteString]
getHeaderValues hname h = [v | (k, v) <- h, k == hname]
