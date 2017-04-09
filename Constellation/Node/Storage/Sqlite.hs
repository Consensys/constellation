{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
module Constellation.Node.Storage.Sqlite where

import ClassyPrelude hiding (fold, hash)
import Control.Monad (void)
import Crypto.Hash (Digest, SHA3_512, hash)
import Data.Binary (encode, decode)
import Data.Pool (createPool, withResource)
import Database.SQLite.Simple
    (Connection, Query, Only(..), open, close, execute, execute_, query, fold_)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as TE
import qualified Text.RawString.QQ as QQ

import Constellation.Enclave.Payload
    (EncryptedPayload(EncryptedPayload, eplCt))
import Constellation.Enclave.Types (PublicKey)
import Constellation.Node.Types
    (Storage(Storage, savePayload, loadPayload, deletePayload, traverseStorage))
import Constellation.Util.Memory (byteArrayToByteString)

createStmts :: [Query]
createStmts =
    [ [QQ.r|
CREATE TABLE cfg
    ( cfgName  TEXT
    , cfgValue TEXT
    )
|]
    , "INSERT INTO cfg (cfgName, cfgValue) VALUES ('dbVersion', '1')"
    , [QQ.r|
CREATE TABLE payload
    ( payloadKey   TEXT PRIMARY KEY
    , payloadBytes BLOB
    )
|]
    ]

sqliteStorage :: FilePath -> IO Storage
sqliteStorage fpath = do
    p      <- createPool (open fpath) close 1 60 5
    exists <- doesFileExist fpath
    unless exists $ withResource p $ \c -> mapM_ (execute_ c) createStmts
    -- TODO: Batching of bursty INSERTs
    -- TODO: Prepare all statements used
    return Storage
        { savePayload     = \epl -> withResource p $ \c -> save c epl
        , loadPayload     = \k   -> withResource p $ \c -> load c k
        , deletePayload   = \k   -> withResource p $ \c -> delete c k
        , traverseStorage = \f   -> withResource p $ \c -> trav c f
        }

save :: Connection -> (EncryptedPayload, [PublicKey]) -> IO (Either String Text)
save c x@(EncryptedPayload{..}, _) = do
    let dig = TE.decodeUtf8 $ B64.encode $
            byteArrayToByteString (hash eplCt :: Digest SHA3_512)
    -- TODO: Error out when the key already exists (collisions)
    execute c
        "INSERT INTO payload (payloadKey, payloadBytes) VALUES (?, ?)"
        (dig, encode x)
    return $ Right dig

load :: Connection -> Text -> IO (Either String (EncryptedPayload, [PublicKey]))
load c k = do
    rs <- query c
        "SELECT payloadBytes FROM payload WHERE payloadKey = ? LIMIT 1"
        (Only k)
    return $ case rs of
        [Only b] -> Right $ decode $ BL.fromStrict b
        []       -> Left "load: No payload found"
        _        -> Left "load: More than one payload found"

delete :: Connection -> Text -> IO ()
delete c k = do
    execute c
        "DELETE FROM payload WHERE payloadKey = ?"
        (Only k)
    return ()

trav :: Connection -> (Text -> (EncryptedPayload, [PublicKey]) -> IO Bool) -> IO ()
trav c f = void $ fold_ c
    "SELECT payloadKey, payloadBytes FROM payload ORDER BY payloadKey DESC"
    True go
  where
    go False _      = return False
    go True  (k, b) = f k (decode $ BL.fromStrict b)
