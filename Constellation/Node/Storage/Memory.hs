{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.Memory where

import ClassyPrelude hiding (delete, hash)
import Crypto.Hash (Digest, SHA3_512, hash)
import Data.ByteArray.Encoding (Base(Base64), convertToBase)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as TE

import Constellation.Enclave.Payload
    (EncryptedPayload(EncryptedPayload, eplCt))
import Constellation.Enclave.Types (PublicKey)
import Constellation.Node.Types
    (Storage(Storage, savePayload, loadPayload, deletePayload,
             traverseStorage, closeStorage))

type Db = TVar (HM.HashMap Text (EncryptedPayload, [PublicKey]))

memoryStorage :: IO Storage
memoryStorage = do
    mvar <- newTVarIO HM.empty
    return Storage
        { savePayload     = save mvar
        , loadPayload     = load mvar
        , deletePayload   = delete mvar
        , traverseStorage = trav mvar
        , closeStorage    = return ()
        }

save :: Db -> (EncryptedPayload, [PublicKey]) -> IO (Either String Text)
save mvar x@(EncryptedPayload{..}, _) = atomically $
    -- TODO: Error out when the key already exists (collisions)
    modifyTVar mvar (HM.insert dig x) >> return (Right dig)
  where
    dig = TE.decodeUtf8 $ convertToBase Base64 (hash eplCt :: Digest SHA3_512)

load :: Db -> Text -> IO (Either String (EncryptedPayload, [PublicKey]))
load mvar k = atomically $ do
    m <- readTVar mvar
    return $ case HM.lookup k m of
        Nothing -> Left "Payload not found in memory database"
        Just v  -> Right v

delete :: Db -> Text -> IO ()
delete mvar k = atomically $ void $ modifyTVar mvar (HM.delete k)

trav :: Db -> (Text -> (EncryptedPayload, [PublicKey]) -> IO Bool) -> IO ()
trav mvar f = atomically (readTVar mvar) >>= loop . HM.toList
  where
    loop []          = return ()
    loop ((k, v):xs) = f k v >>= \cont -> when cont $ loop xs
