{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Memory where

import ClassyPrelude

import qualified Data.ByteArray as BA
import qualified Data.ByteString as B

-- | WARNING: This function is unsafe when used on any kind of Master Key,
-- Private Key, etc. as the bytes will be copied all over memory.
-- Appropriate/typical use is to convert a hash digest from cryptonite
-- to a ByteString.
byteArrayToByteString :: BA.ByteArrayAccess a => a -> ByteString
byteArrayToByteString = B.pack . BA.unpack
