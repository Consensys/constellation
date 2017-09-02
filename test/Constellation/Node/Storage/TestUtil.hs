{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.TestUtil where

import ClassyPrelude
import Test.Tasty.HUnit ((@?), (@?=), Assertion)
import qualified Data.ByteString.Char8 as BC

import Constellation.Enclave.Key (newKeyPair)
import Constellation.Enclave.Payload (encrypt)
import Constellation.Enclave.Types (PublicKey(unPublicKey))
import Constellation.Node.Types (Storage(loadPayload, savePayload, deletePayload,
                                         closeStorage))

testStorage :: Storage -> String -> ((String -> IO ()) -> Assertion)
testStorage storage testName step = do
    step "Saving payload"
    (pub1, boxPriv1) <- newKeyPair
    (pub2, _)        <- newKeyPair
    let boxPub1 = unPublicKey pub1
        boxPub2 = unPublicKey pub2
    epl <- encrypt (BC.pack "payload") boxPub1 boxPriv1 [boxPub2]
    let kv = (epl, [pub2])
    ek  <- savePayload storage kv
    key <- case ek of
        Left err  -> error $ testName ++ ": Saving of payload failed: " ++ err
        Right key -> return key
    step "Retrieving payload"
    ret <- loadPayload storage key
    case ret of
        Left err -> error $ testName ++ ": Retrieval of payload failed: " ++ err
        Right r  -> r @?= kv
    step "Deleting payload"
    _ <- deletePayload storage key
    step "Verify deletion"
    ver <- loadPayload storage key
    case ver of
        Left err -> "Payload not found in " `isInfixOf` err @? testName ++ ": Key still present"
        Right _  -> error $ testName ++ ": Deletion of payload failed"
    step "Cleaning up"
    closeStorage storage
