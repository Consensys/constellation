{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.Sqlite where

import ClassyPrelude hiding (fold, delete, hash)
import Control.Monad (void)
import Crypto.Hash (Digest, SHA3_512, hash)
import Data.Binary (encode, decode)
import Data.ByteArray.Encoding (Base(Base64), convertToBase)
import Data.Pool (createPool, withResource, destroyAllResources)
import Database.SQLite.Simple
    (Connection, Query, Only(..), open, close, execute, execute_, query, fold_)
import System.Directory (doesFileExist)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Text.RawString.QQ as QQ

import Constellation.Enclave.Payload
    (EncryptedPayload(EncryptedPayload, eplCt))
import Constellation.Enclave.Types (PublicKey)
import Constellation.Node.Types (Storage(..))

createStmts :: [Query]
createStmts =
    [ [QQ.r|
CREATE TABLE cfg
    ( name  TEXT
    , value TEXT
    )
|]
    , "INSERT INTO cfg (name, value) VALUES ('dbVersion', '1')"
    , [QQ.r|
CREATE TABLE payload
    ( key   TEXT PRIMARY KEY
    , bytes BLOB
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
        , closeStorage    = destroyAllResources p
        }

save :: Connection -> (EncryptedPayload, [PublicKey]) -> IO (Either String Text)
save c x@(EncryptedPayload{..}, _) = do
    let dig = TE.decodeUtf8 $ convertToBase Base64 (hash eplCt :: Digest SHA3_512)
    -- TODO: Error out when the key already exists (collisions)
    execute c
        "INSERT INTO payload (key, bytes) VALUES (?, ?)"
        (dig, encode x)
    return $ Right dig

load :: Connection -> Text -> IO (Either String (EncryptedPayload, [PublicKey]))
load c k = do
    rs <- query c
        "SELECT bytes FROM payload WHERE key = ? LIMIT 1"
        (Only k)
    return $ case rs of
        [Only b] -> Right $ decode $ BL.fromStrict b
        -- []       -> Left "load: No payload found"
        -- _        -> Left "load: More than one payload found"
        -- TODO: In testStorage, don't rely on the presence of the strings below
        []       -> Left "Payload not found in SQLite"
        _        -> Left "Payload not found in SQLite"

delete :: Connection -> Text -> IO ()
delete c k = void $
    execute c
        "DELETE FROM payload WHERE key = ?"
        (Only k)

trav :: Connection -> (Text -> (EncryptedPayload, [PublicKey]) -> IO Bool) -> IO ()
trav c f = void $ fold_ c
    "SELECT key, bytes FROM payload ORDER BY key DESC"
    True go
  where
    go False _      = return False
    go True  (k, b) = f k (decode $ BL.fromStrict b)
