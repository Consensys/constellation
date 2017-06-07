{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.ByteString where

import ClassyPrelude
import Data.ByteArray.Encoding (Base(Base64), convertToBase, convertFromBase)
import qualified Data.Text.Encoding as TE

import Constellation.Util.Either (fromRight)

b64TextEncodeBs :: ByteString -> Text
b64TextEncodeBs = TE.decodeUtf8 . convertToBase Base64

b64TextDecodeBs :: Text -> Either String ByteString
b64TextDecodeBs = convertFromBase Base64 . TE.encodeUtf8

mustB64TextDecodeBs :: Text -> ByteString
mustB64TextDecodeBs = fromRight . b64TextDecodeBs

mustB64DecodeBs :: ByteString -> ByteString
mustB64DecodeBs = fromRight . convertFromBase Base64
