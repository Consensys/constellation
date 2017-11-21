{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Constellation.Util.Json where

import ClassyPrelude
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Hex (unhex)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as TE

import Constellation.Util.ByteString
    (b64TextEncodeBs, b64TextDecodeBs, hexWithColons)

newtype JsonBs = JsonBs { unJsonBs :: ByteString }

instance FromJSON JsonBs where
    parseJSON (String s) = case b64TextDecodeBs s of
        Left err -> fail err
        Right b  -> return $ JsonBs b
    parseJSON _          = mzero

instance ToJSON JsonBs where
    toJSON = toJSON . b64TextEncodeBs . unJsonBs

newtype HexBs = HexBs { unHexBs :: ByteString }

instance FromJSON HexBs where
    parseJSON (String s) = HexBs <$> unhex (BC.filter (/= ':') (TE.encodeUtf8 s))
    parseJSON _          = mzero

instance ToJSON HexBs where
    toJSON = toJSON . TE.decodeUtf8 . hexWithColons . unHexBs
