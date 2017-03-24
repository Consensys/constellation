{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
module Constellation.TestUtil where

import ClassyPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), testCase)
import qualified Data.HashMap.Strict as HM

import Constellation.Enclave
    (newEnclave', enclaveEncryptPayload, enclaveDecryptPayload)
import Constellation.Enclave.Key (newKeyPair)
import Constellation.Enclave.Types (PublicKey)
import Constellation.Node
    (newNode, addParty, sendPayload, receivePayload)
import Constellation.Node.Storage.BerkeleyDb (berkeleyDbStorage)
import Constellation.Node.Types
    ( Node(Node, nodePi), PartyInfo(piUrl, piRcpts)
    , Crypt(Crypt, encryptPayload, decryptPayload)
    )
import Constellation.Util.Network (getUnusedPort)
import Constellation.Util.Text (tformat)

setupTestNode :: FilePath -> String -> IO (TVar Node, Int)
setupTestNode d name = do
    kp1@(pub1, _) <- newKeyPair
    kp2@(pub2, _) <- newKeyPair
    e             <- newEnclave' [kp1, kp2]
    let crypt = Crypt
            { encryptPayload = enclaveEncryptPayload e
            , decryptPayload = enclaveDecryptPayload e
            }
    storage <- berkeleyDbStorage $ d </> name
    port    <- getUnusedPort
    nvar    <- newTVarIO =<<
        newNode crypt storage (tformat "http://localhost:{}/" [port])
        [pub1, pub2] []
    return (nvar, port)

link :: TVar Node -> TVar Node -> STM ()
link nvar onvar = readTVar onvar >>= \onode ->
    addParty (piUrl $ nodePi onode) nvar

testSendPayload :: TVar Node -> TVar Node -> Assertion
testSendPayload nvar onvar = do
    [node, onode] <- atomically $ mapM readTVar [nvar, onvar]
    let pl   = "foo bar baz"
        from = firstPublicKey node
        to   = firstPublicKey onode
    es <- sendPayload node pl from [to]
    let key = case partitionEithers es of
            ([], [k]) -> k
            _         -> error "testSendPayload: Invalid response from sendPayload"
    -- Verify that the recipient node can retrieve and decrypt the payload
    epl <- receivePayload onode key to
    case epl of
        Left err  -> error $ "testSendPayload: Recipient: Error receiving payload: " ++ err
        Right pl' -> pl' @?= pl
    -- Verify that the sender node can retrieve and decrypt the payload
    epl' <- receivePayload node key to
    case epl' of
        Left err  -> error $ "testSendPayload: Sender: Error receiving payload: " ++ err
        Right pl' -> pl' @?= pl

firstPublicKey :: Node -> PublicKey
firstPublicKey Node{..} = case ownPubs of
    ((pub, _):_) -> pub
    _            -> error "firstPublicKey: Not found"
  where
    ownPubs = filter (\(_, url) -> url == piUrl nodePi)
        (HM.toList $ piRcpts nodePi)

kvTest :: (Show a, Eq b, Show b) => String -> [(a, b)] -> (a -> b) -> TestTree
kvTest name tests f = testGroup name $ map gen tests
  where
    gen (input, output) = testCase (show input) $ f input @?= output
