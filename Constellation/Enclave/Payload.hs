{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Constellation.Enclave.Payload where

import ClassyPrelude
import Data.Binary (Binary(put, get))
import Data.Maybe (fromJust)
import qualified Crypto.Saltine.Class as S
import qualified Crypto.Saltine.Core.Box as Box
import qualified Crypto.Saltine.Core.SecretBox as SBox

data EncryptedPayload = EncryptedPayload
    { eplSender    :: !Box.PublicKey
    , eplCt        :: !ByteString
    , eplNonce     :: !SBox.Nonce
    , eplRcptBoxes :: ![ByteString]
    , eplRcptNonce :: !Box.Nonce
    }

instance Show EncryptedPayload where
    show = show . encodeable

encodeable :: EncryptedPayload
           -> (ByteString, ByteString, ByteString, [ByteString], ByteString)
encodeable EncryptedPayload{..} =
    ( S.encode eplSender
    , eplCt
    , S.encode eplNonce
    , eplRcptBoxes
    , S.encode eplRcptNonce
    )

instance Binary EncryptedPayload where
    put = put . encodeable
    get = get >>= \(sender, ct, nonce, rcptBoxes, rcptNonce) -> return EncryptedPayload
        { eplSender    = fromJust $ S.decode sender
        , eplCt        = ct
        , eplNonce     = fromJust $ S.decode nonce
        , eplRcptBoxes = rcptBoxes
        , eplRcptNonce = fromJust $ S.decode rcptNonce
        }

encrypt :: ByteString
        -> Box.PublicKey
        -> Box.SecretKey
        -> [Box.PublicKey]
        -> IO EncryptedPayload
encrypt pl sender pk rcpts = encrypt' pl sender cks
  where
    cks = map (safeBeforeNM sender pk) rcpts

safeBeforeNM :: Box.PublicKey -> Box.SecretKey -> Box.PublicKey -> Box.CombinedKey
safeBeforeNM sender pk rcpt
    | sender == rcpt = error "encrypt: Sender cannot be a recipient"
    | otherwise      = Box.beforeNM pk rcpt

encrypt' :: ByteString
         -> Box.PublicKey
         -> [Box.CombinedKey]
         -> IO EncryptedPayload
encrypt' pl sender cks = do
    (mk, nonce, ct) <- sboxSeal pl
    rcptNonce       <- Box.newNonce
    let rcptBoxes = map (\ck -> Box.boxAfterNM ck rcptNonce emk) cks
        emk       = S.encode mk
    return EncryptedPayload
        { eplSender    = sender
        , eplCt        = ct
        , eplNonce     = nonce
        , eplRcptBoxes = rcptBoxes
        , eplRcptNonce = rcptNonce
        }

sboxSeal :: ByteString -> IO (SBox.Key, SBox.Nonce, ByteString)
sboxSeal pt = do
    nonce <- SBox.newNonce
    mk    <- SBox.newKey
    let ct = SBox.secretbox mk nonce pt
    return (mk, nonce, ct)

decrypt :: ByteString
        -> SBox.Nonce
        -> ByteString
        -> Box.Nonce
        -> Box.PublicKey
        -> Box.SecretKey
        -> Maybe ByteString
decrypt ct nonce rcptBox rcptNonce senderPub pk =
    decrypt' ct nonce rcptBox rcptNonce ck
  where
    ck = Box.beforeNM pk senderPub

decrypt' :: ByteString
         -> SBox.Nonce
         -> ByteString
         -> Box.Nonce
         -> Box.CombinedKey
         -> Maybe ByteString
decrypt' ct nonce rcptBox rcptNonce ck =
    case Box.boxOpenAfterNM ck rcptNonce rcptBox of
        Nothing -> Nothing
        Just emk -> case S.decode emk of
            Nothing -> Nothing
            Just mk -> SBox.secretboxOpen mk nonce ct
