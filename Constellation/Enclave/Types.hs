{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Enclave.Types where

import ClassyPrelude
import Data.Aeson (FromJSON(parseJSON))
import Data.Binary (Binary(put, get))
import Data.ByteArray.Encoding (Base(Base64), convertToBase)
import Data.Hashable (Hashable(hashWithSalt))
import Data.Maybe (fromJust)
import qualified Crypto.Saltine.Class as S
import qualified Crypto.Saltine.Core.Box as Box
import qualified Data.Aeson as AE

import Constellation.Util.ByteString (b64TextDecodeBs)

newtype PublicKey = PublicKey { unPublicKey :: Box.PublicKey }
                  deriving (Eq)

instance Show PublicKey where
    show (PublicKey pub) =
        show (convertToBase Base64 (S.encode pub) :: ByteString)

instance Binary PublicKey where
    put = put . S.encode . unPublicKey
    get = (PublicKey . fromJust . S.decode) <$> get

instance Hashable PublicKey where
    hashWithSalt salt (PublicKey pub) = hashWithSalt salt (S.encode pub)

instance FromJSON PublicKey where
    parseJSON (AE.String s) = case b64TextDecodeBs s of
        Left err -> fail err
        Right bs -> case mkPublicKey bs of
            Nothing  -> fail "Failed to mkPublicKey"
            Just pub -> return pub
    parseJSON _             = fail "PublicKey must be an Aeson String"

mkPublicKey :: ByteString -> Maybe PublicKey
mkPublicKey bs = PublicKey <$> S.decode bs
