{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.Directory where

import ClassyPrelude hiding (delete, hash)
import Crypto.Hash (Digest, SHA3_512, hash)
import Data.Binary (encode, decode)
import Data.ByteArray.Encoding
    (Base(Base32, Base64), convertToBase, convertFromBase)
import System.Directory
    (createDirectoryIfMissing, listDirectory, doesFileExist, removeFile)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE

import Constellation.Enclave.Payload
    (EncryptedPayload(EncryptedPayload, eplCt))
import Constellation.Enclave.Types (PublicKey)
import Constellation.Node.Types
    (Storage(Storage, savePayload, loadPayload, deletePayload,
             traverseStorage, closeStorage))
import Constellation.Util.Exception (trys)
import Constellation.Util.File (writeFileLbs)
import Constellation.Util.Logging (warnf)

directoryStorage :: FilePath -> IO Storage
directoryStorage dir = do
    createDirectoryIfMissing True (dir </> "payloads")
    return Storage
        { savePayload     = save dir
        , loadPayload     = load dir
        , deletePayload   = delete dir
        , traverseStorage = trav dir
        , closeStorage    = return ()
        }

save :: FilePath -> (EncryptedPayload, [PublicKey]) -> IO (Either String Text)
save dir x@(EncryptedPayload{..}, _) = doesFileExist fpath >>= \exists ->
    if exists
        then return $ Left "save: Payload file already exists"
        else trys $ writeFileLbs fpath (encode x) >> return k
          where
            fpath = dir </> "payloads" </> fname
            fname = BC.unpack $ convertToBase Base32 dig
            k     = TE.decodeUtf8 $ convertToBase Base64 dig
            dig   = hash eplCt :: Digest SHA3_512

load :: FilePath -> Text -> IO (Either String (EncryptedPayload, [PublicKey]))
load dir k = case convertFromBase Base64 $ TE.encodeUtf8 k of
    Left err  -> return $ Left err
    Right dig -> load' dir (BC.unpack $ convertToBase Base32 (dig :: ByteString))

load' :: FilePath
      -> FilePath
      -> IO (Either String (EncryptedPayload, [PublicKey]))
load' dir fname = do
    ex <- trys $ decode <$> BL.readFile (dir </> "payloads" </> fname)
    return $ case ex of
        Left err -> Left $ "Payload not found in directory " ++ dir ++ ": " ++ err
        Right x  -> Right x

delete :: FilePath -> Text -> IO ()
delete dir k = case convertFromBase Base64 $ TE.encodeUtf8 k of
    Left err  -> warnf "Invalid/non-Base64 key '{}' given to delete: {}" (k, err)
    Right dig -> do
        let fname = BC.unpack (convertToBase Base32 (dig :: ByteString))
        removeFile (dir </> "payloads" </> fname)

trav :: FilePath -> (Text -> (EncryptedPayload, [PublicKey]) -> IO Bool) -> IO ()
trav dir f = listDirectory dir >>= loop
  where
    loop []         = return ()
    loop (fname:xs) = case reverse $ take 4 $ reverse fname of
        ".tmp" -> return ()
        _      -> case convertFromBase Base32 (BC.pack fname) of
            Left err  -> do
                warnf "Invalid/non-Base32 file '{}': {}" (fname, err)
                loop xs
            Right dig -> do
                let k = TE.decodeUtf8 $ convertToBase Base64 (dig :: ByteString)
                load' dir fname >>= \case
                    Left err -> do
                        warnf "Failed to load payload {} during directory traversal: {}" (k, err)
                        loop xs
                    Right x  -> f k x >>= \cont -> when cont $ loop xs
