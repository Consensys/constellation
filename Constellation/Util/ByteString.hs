{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Constellation.Util.ByteString where

import ClassyPrelude
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE

import Constellation.Util.Either (fromRight)

b64TextEncodeBs :: ByteString -> Text
b64TextEncodeBs = TE.decodeUtf8 . B64.encode

b64TextDecodeBs :: Text -> Either String ByteString
b64TextDecodeBs = B64.decode . TE.encodeUtf8

mustB64TextDecodeBs :: Text -> ByteString
mustB64TextDecodeBs = fromRight . b64TextDecodeBs

mustB64DecodeBs :: ByteString -> ByteString
mustB64DecodeBs = fromRight . B64.decode
