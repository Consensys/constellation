{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Enclave where

import ClassyPrelude
import qualified Crypto.Saltine.Core.Box as Box
import qualified Data.HashMap.Strict as HM

import Constellation.Enclave.Key (loadKeyPairs)
import Constellation.Enclave.Payload
    ( EncryptedPayload(EncryptedPayload, eplSender, eplCt, eplNonce
                      , eplRcptBoxes, eplRcptNonce)
    , safeBeforeNM, encrypt', decrypt'
    )
import Constellation.Enclave.Types (PublicKey(PublicKey), unPublicKey)
import Constellation.Util.Exception (trys)

data Enclave = Enclave
    { enclaveKeys       :: HM.HashMap PublicKey Box.SecretKey
    , enclaveComboCache :: TVar (HM.HashMap (PublicKey, PublicKey) Box.CombinedKey)
    }

newEnclave :: [(FilePath, FilePath, Maybe String)] -> IO (Either String Enclave)
newEnclave ks = loadKeyPairs ks >>= \case
    Left err  -> return $ Left err
    Right kps -> Right <$> newEnclave' kps

newEnclave' :: [(PublicKey, Box.SecretKey)] -> IO Enclave
newEnclave' keyPairs = do
    cvar <- newTVarIO HM.empty
    return Enclave
        { enclaveKeys       = HM.fromList keyPairs
        , enclaveComboCache = cvar
        }

enclaveEncryptPayload :: Enclave
                      -> ByteString
                      -> PublicKey
                      -> [PublicKey]
                      -> IO (Either String EncryptedPayload)
enclaveEncryptPayload e pl sender rcpts = do
    ecks <- trys $ atomically $ getCombinedKeys e sender rcpts
    case ecks of
        Left err  -> return $ Left err
        Right cks -> Right <$> encrypt' pl (unPublicKey sender) cks

getCombinedKeys :: Enclave
                -> PublicKey
                -> [PublicKey]
                -> STM [Box.CombinedKey]
getCombinedKeys Enclave{..} sender rcpts = do
    curCache <- readTVar enclaveComboCache
    let (cks, finalCc, ccChanged) = foldr get ([], curCache, False) rcpts
        get rcpt (ks, cc, chd)    = case HM.lookup (sender, rcpt) cc of
            Nothing -> case HM.lookup sender enclaveKeys of
                Nothing -> error "getCombinedKeys: Matching private key not found"
                Just pk -> (k:ks, ncc, True)
                  where
                    !k  = safeBeforeNM (unPublicKey sender) pk (unPublicKey rcpt)
                    ncc = HM.insert (sender, rcpt) k cc
            Just k  -> (k:ks, cc, chd)
    when ccChanged $ writeTVar enclaveComboCache finalCc
    return cks

enclaveDecryptPayload :: Enclave
                      -> EncryptedPayload
                      -> PublicKey
                      -> IO (Either String ByteString)
enclaveDecryptPayload e EncryptedPayload{..} rcptPub = do
    ecks <- trys $ atomically $ getCombinedKeys e rcptPub [PublicKey eplSender]
    case ecks of
        Left err   -> return $ Left err
        Right []   -> return $ Left "enclaveDecrypt: No CombinedKey found"
        Right [ck] -> case eplRcptBoxes of
            (rcptBox:_) -> case decrypt' eplCt eplNonce rcptBox eplRcptNonce ck of
                Nothing -> return $ Left "enclaveDecrypt: Decryption failed"
                Just pt -> return $ Right pt
            _           -> return $ Left "enclaveDecrypt: No rcptBox found"
        _          -> return $ Left "enclaveDecrypt: More than one CombinedKey found"
