{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Types where

import ClassyPrelude
import Data.Binary (Binary(put, get))
import Network.HTTP.Conduit (Manager)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Constellation.Enclave.Payload (EncryptedPayload)
import Constellation.Enclave.Types (PublicKey)

data Node = Node
    { nodePi           :: PartyInfo
    , nodeCrypt        :: Crypt
    , nodeStorage      :: Storage
    , nodeDefaultPub   :: Maybe PublicKey
    , nodeAlwaysSendTo :: [PublicKey]
    , nodeSelfPub      :: PublicKey
    , nodeSetSecure    :: Bool
    , nodeManager      :: Manager
    }

data PartyInfo = PartyInfo
    { piUrl     :: Text
    , piRcpts   :: HM.HashMap PublicKey Text
    , piParties :: HS.HashSet Text
    } deriving (Show)

instance Binary PartyInfo where
    put PartyInfo{..} = put (piUrl, HM.toList piRcpts, HS.toList piParties)
    get               = get >>= \(url, rcpts, parties) -> return PartyInfo
        { piUrl     = url
        , piRcpts   = HM.fromList rcpts
        , piParties = HS.fromList parties
        }

data Crypt = Crypt
    { encryptPayload :: ByteString
                     -> PublicKey
                     -> [PublicKey]
                     -> IO (Either String EncryptedPayload)
    , decryptPayload :: EncryptedPayload
                     -> PublicKey
                     -> IO (Either String ByteString)
    }

data Storage = Storage
    { savePayload     :: (EncryptedPayload, [PublicKey])
                      -> IO (Either String Text)
    , loadPayload     :: Text
                      -> IO (Either String (EncryptedPayload, [PublicKey]))
    , deletePayload   :: Text -> IO ()
    , traverseStorage :: (Text -> (EncryptedPayload, [PublicKey]) -> IO Bool)
                      -> IO ()
    , closeStorage    :: IO ()
    }
