{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.LevelDb where

import ClassyPrelude hiding (hash)
import Control.Monad.Fix (fix)
import Crypto.Hash (Digest, SHA3_512, hash)
import Data.Binary (encode, decode)
import Data.Default (def)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Database.LevelDB.Base as L
import qualified Database.LevelDB.Internal as LI

import Constellation.Enclave.Payload
    (EncryptedPayload(EncryptedPayload, eplCt))
import Constellation.Enclave.Types (PublicKey)
import Constellation.Node.Types
    (Storage(Storage, savePayload, loadPayload, traverseStorage, closeStorage))
import Constellation.Util.Memory (byteArrayToByteString)

levelDbStorage :: FilePath -> IO Storage
levelDbStorage fpath = do
    db <- L.open fpath def
        { L.createIfMissing = True
        }
    return Storage
        { savePayload     = save db
        , loadPayload     = load db
        , traverseStorage = trav db
        , closeStorage    = LI.unsafeClose db
        }

save :: L.DB -> (EncryptedPayload, [PublicKey]) -> IO (Either String Text)
save db x@(EncryptedPayload{..}, _) =
    -- TODO: Error out when the key already exists (collisions)
    L.put db def dig (BL.toStrict $ encode x) >>
    return (Right $ TE.decodeUtf8 dig)
  where
    dig = B64.encode $ byteArrayToByteString (hash eplCt :: Digest SHA3_512)

load :: L.DB -> Text -> IO (Either String (EncryptedPayload, [PublicKey]))
load db = L.get db def . TE.encodeUtf8 >=> \mv -> return $ case mv of
    Nothing -> Left "Key not found in LevelDB"
    Just v  -> Right $ decode $ BL.fromStrict v

trav :: L.DB -> (Text -> (EncryptedPayload, [PublicKey]) -> IO Bool) -> IO ()
trav db f = L.withIter db def $ \it -> fix $ \loop -> do
    mk <- L.iterKey it
    case mk of
        Nothing -> return ()
        Just k  -> do
            mv <- L.iterValue it
            case mv of
                Nothing -> return ()  -- TODO: Does this happen?
                Just v  -> do
                    cont <- f (TE.decodeUtf8 k) $ decode $ BL.fromStrict v
                    when cont $ L.iterNext it >> loop
