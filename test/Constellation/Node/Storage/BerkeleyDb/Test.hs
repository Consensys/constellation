{-# LANGUAGE NoImplicitPrelude #-}
module Constellation.Node.Storage.BerkeleyDb.Test where

import ClassyPrelude
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCaseSteps)
import qualified Data.ByteString.Char8 as C

import Constellation.Enclave.Key (newKeyPair)
import Constellation.Enclave.Payload (encrypt)
import Constellation.Enclave.Types (PublicKey(unPublicKey))
import Constellation.Node.Storage.BerkeleyDb (berkeleyDbStorage)
import Constellation.Node.Types (Storage(loadPayload, savePayload, deletePayload,
                                         closeStorage))

tests :: TestTree
tests = testGroup "Node.Storage.BerkeleyDb"
    [ testBerkeleyDb
    ]

testBerkeleyDb :: TestTree
testBerkeleyDb = testCaseSteps "delete" $ \step ->
    withSystemTempDirectory "constellation-test-XXX" $ \tempDir-> do
        step "Setting up BerkeleyDb instance"
        storage <- berkeleyDbStorage tempDir

        step "Saving payload"
        (pub1, boxPriv1) <- newKeyPair
        (pub2, _) <- newKeyPair
        let boxPub1 = unPublicKey pub1
        let boxPub2 = unPublicKey pub2
        epl <- encrypt (C.pack "payload") boxPub1 boxPriv1 [boxPub2]
        let kv = (epl, [pub2])
        ek <- savePayload storage kv
        key <- case ek of
            Left err -> error $ "testBerkeleyDb: Saving of payload failed: " ++ err
            Right key -> return key

        step "Retrieving payload"
        ret <- loadPayload storage key
        case ret of
            Left err -> error $ "testBerkeleyDb: Retrieval of payload failed: " ++ err
            Right r -> r @?= kv

        step "Deleting payload"
        _ <- deletePayload storage key
        
        step "Verify deletion"
        ver <- loadPayload storage key
        case ver of
            Left err -> err @?= "Key not found in BerkeleyDb payload.db"
            Right _ -> error $ "testBerkeleyDb: Deletion of payload failed"
        
        step "Cleaning up"
        closeStorage storage
