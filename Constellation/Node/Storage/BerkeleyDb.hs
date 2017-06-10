{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.BerkeleyDb where

import ClassyPrelude hiding (delete, hash)
import Control.Logging (warn, warnS)
import Crypto.Hash (Digest, SHA3_512, hash)
import Data.Binary (encode, decode)
import Data.ByteArray.Encoding (Base(Base64), convertToBase)
import Database.Berkeley.Db
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Constellation.Enclave.Payload
    (EncryptedPayload(EncryptedPayload, eplCt))
import Constellation.Enclave.Types (PublicKey)
import Constellation.Node.Types
    (Storage(Storage, savePayload, loadPayload, deletePayload,
             traverseStorage, closeStorage))

berkeleyDbStorage :: FilePath -> IO Storage
berkeleyDbStorage fpath = do
    createDirectoryIfMissing True fpath
    dbEnv <- dbEnv_create []
    -- dbEnv_set_cache_size dbEnv 1 0 1  -- 1GB + 0 bytes in 1 continuous region
    dbEnv_set_cache_size dbEnv 0 (256 * 1024 * 1024) 1  -- 256 MB in 1 continuous region
    dbEnv_set_lg_regionmax dbEnv 8388608  -- 8MB
    dbEnv_set_lk_detect dbEnv DB_LOCK_DEFAULT
    dbEnv_open
        [ DB_CREATE
        , DB_INIT_LOCK
        , DB_INIT_LOG
        , DB_INIT_MPOOL
        , DB_INIT_TXN
        , DB_THREAD
        , DB_RECOVER
        , DB_PRIVATE
        ] 0 dbEnv fpath
    db <- db_create [] dbEnv
    db_set_pagesize db 65536  -- 64 KB
    dbEnv_withTxn [] [] dbEnv Nothing $ \tx ->
        db_open
            [ DB_CREATE
            , DB_READ_UNCOMMITTED
            ] DB_HASH 0 db (Just tx) "payload.db" Nothing
    -- Reads in READ_UNCOMMITTED mode should never throw deadlock exceptions
    -- let r   = withRetrying dbEnv Nothing 10000
    let rtx = withRetryingTx dbEnv Nothing 10000
    return Storage
        { savePayload     = \epl -> rtx $ \tx -> save db (Just tx) epl
        , loadPayload     = load db Nothing
        , deletePayload   = delete db Nothing
        , traverseStorage = trav db Nothing
        , closeStorage    = do
              db_close [] db
              dbEnv_close [] dbEnv
        }

-- withRetrying :: DbEnv -> Maybe DbTxn -> Int -> IO a -> IO a
-- withRetrying dbEnv mparentTx retries f = catch
--     (do
--           v <- f
--           return v
--     )
--     (\e -> do
--           warnS "withRetrying: Exception: "
--               (T.pack $ show e)
--           case fromException e of
--               Just (DbException _ DB_LOCK_DEADLOCK)
--                   | retries > 1 -> do
--                         warn "withRetrying: Retrying deadlocked BerkeleyDb operation"
--                         withRetrying dbEnv mparentTx (pred retries) f
--               _                 -> throwIO e
--     )

withRetryingTx :: DbEnv -> Maybe DbTxn -> Int -> (DbTxn -> IO a) -> IO a
withRetryingTx dbEnv mparentTx retries f = do
    tx <- dbEnv_txn_begin [] dbEnv mparentTx
    catch
        (do
              v <- f tx
              dbTxn_commit [] tx
              return v
        )
        (\e -> do
              warnS "withRetryingTx: Exception in transaction: "
                  (T.pack $ show e)
              dbTxn_abort tx
              case fromException e of
                  Just (DbException _ DB_LOCK_DEADLOCK)
                      | retries > 1 -> do
                            warn "withRetryingTx: Retrying deadlocked BerkeleyDb transaction"
                            withRetryingTx dbEnv mparentTx (pred retries) f
                  _                 -> throwIO e
        )

save :: Db
     -> Maybe DbTxn
     -> (EncryptedPayload, [PublicKey])
     -> IO (Either String Text)
save db mtx x@(EncryptedPayload{..}, _) =
    -- TODO: Error out when the key already exists (collisions)
    db_put [] db mtx dig (BL.toStrict $ encode x) >>
    return (Right $ TE.decodeUtf8 dig)
  where
    dig = convertToBase Base64 (hash eplCt :: Digest SHA3_512) :: ByteString

load :: Db
     -> Maybe DbTxn
     -> Text
     -> IO (Either String (EncryptedPayload, [PublicKey]))
load db mtx k = do
    mv <- db_get [DB_READ_UNCOMMITTED] db mtx (TE.encodeUtf8 k)
    return $ case mv of
        Nothing -> Left "Payload not found in BerkeleyDb payload.db"
        Just v  -> Right $ decode $ BL.fromStrict v

delete :: Db
     -> Maybe DbTxn
     -> Text
     -> IO ()
delete db mtx k = void $ db_del [] db mtx (TE.encodeUtf8 k)

trav :: Db
     -> Maybe DbTxn
     -> (Text -> (EncryptedPayload, [PublicKey]) -> IO Bool)
     -> IO ()
trav _ _ _ = return ()  -- TODO
