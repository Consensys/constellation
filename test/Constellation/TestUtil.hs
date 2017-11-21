{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.TestUtil where

import ClassyPrelude
import Network.Wai.Handler.Warp (setPort, defaultSettings)
import Network.Wai.Handler.WarpTLS (TLSSettings, runTLS)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), testCase)
import qualified Data.HashMap.Strict as HM

import Constellation.Enclave
    (newEnclave', enclaveEncryptPayload, enclaveDecryptPayload)
import Constellation.Enclave.Key (newKeyPair)
import Constellation.Enclave.Types (PublicKey)
import Constellation.Node
    (newNode, addParty, nodeRefresh, sendPayload, receivePayload)
import Constellation.Node.Api (ApiType(..), apiApp)
import Constellation.Node.Main (setupServerTls, setupClientTls)
import Constellation.Node.Storage.Directory (directoryStorage)
import Constellation.Node.Trust (TrustMode(..))
import Constellation.Node.Types
    ( Node(Node, nodePi, nodeStorage), PartyInfo(piUrl, piRcpts)
    , Crypt(Crypt, encryptPayload, decryptPayload), Storage(closeStorage)
    )
import Constellation.Util.Network (getUnusedPort)
import Constellation.Util.String (truncateString)
import Constellation.Util.Text (tformat)

import Constellation.MockData

withThreeTestNodes :: ((TVar Node, TVar Node, TVar Node) -> Assertion)
                   -> Assertion
withThreeTestNodes = withThreeTestNodes' (t, t, t)
    where
      t = (NoValidation, NoValidation)

withThreeTestNodes' :: ( (TrustMode, TrustMode)
                       , (TrustMode, TrustMode)
                       , (TrustMode, TrustMode)
                       )
                    -> ((TVar Node, TVar Node, TVar Node) -> Assertion)
                    -> Assertion
withThreeTestNodes' (t1, t2, t3) f = withThreeTempDirs $ \(d1, d2, d3) -> do
    writeFile (d1 </> "tls-server-cert.pem") testingOnlyMockServerCert1
    writeFile (d1 </> "tls-server-key.pem") testingOnlyMockServerKey1
    writeFile (d1 </> "tls-client-cert.pem") testingOnlyMockClientCert1
    writeFile (d1 </> "tls-client-key.pem") testingOnlyMockClientKey1
    writeFile (d2 </> "tls-server-cert.pem") testingOnlyMockServerCert2
    writeFile (d2 </> "tls-server-key.pem") testingOnlyMockServerKey2
    writeFile (d2 </> "tls-client-cert.pem") testingOnlyMockClientCert2
    writeFile (d2 </> "tls-client-key.pem") testingOnlyMockClientKey2
    writeFile (d3 </> "tls-server-cert.pem") testingOnlyMockServerCert3
    writeFile (d3 </> "tls-server-key.pem") testingOnlyMockServerKey3
    writeFile (d3 </> "tls-client-cert.pem") testingOnlyMockClientCert3
    writeFile (d3 </> "tls-client-key.pem") testingOnlyMockClientKey3
    forM_ [d1, d2, d3] $ \d ->
        setFileMode (d </> "tls-server-key.pem") 33152 >>
        setFileMode (d </> "tls-client-key.pem") 33152
    (nvar1, port1, tls1) <- setupTestNode t1 d1
    (nvar2, port2, tls2) <- setupTestNode t2 d2
    (nvar3, port3, tls3) <- setupTestNode t3 d3
    tid1 <- fork $ runTLS tls1
        (setPort port1 defaultSettings) $ apiApp Nothing Public nvar1
    tid2 <- fork $ runTLS tls2
        (setPort port2 defaultSettings) $ apiApp Nothing Public nvar2
    tid3 <- fork $ runTLS tls3
        (setPort port3 defaultSettings) $ apiApp Nothing Public nvar3
    atomically $ do
        link nvar1 nvar2
        link nvar2 nvar3
    let nvars = [nvar1, nvar2, nvar3]
    mapM_ nodeRefresh nvars  -- Once to discover the first nodes
    mapM_ nodeRefresh nvars  -- Twice to receive updated info
    mapM_ nodeRefresh nvars  -- Thrice to check that nothing is crazy
    f (nvar1, nvar2, nvar3)
    killThread tid1
    killThread tid2
    killThread tid3
    nodes <- atomically $ mapM readTVar [nvar1, nvar2, nvar3]
    mapM_ (closeStorage . nodeStorage) nodes

withThreeTempDirs :: (MonadIO m, MonadMask m)
                  => ((FilePath, FilePath, FilePath) -> m a)
                  -> m a
withThreeTempDirs f =
    withTempDir $ \d1 ->
        withTempDir $ \d2 ->
            withTempDir $ \d3 -> f (d1, d2, d3)

withTempDir :: (MonadIO m, MonadMask m) => (FilePath -> m a) -> m a
withTempDir = withSystemTempDirectory "c11n-test-"

setupTestNode :: (TrustMode, TrustMode)
              -> FilePath
              -> IO (TVar Node, Int, TLSSettings)
setupTestNode (strust, ctrust) d = do
    kp1@(pub1, _) <- newKeyPair
    kp2@(pub2, _) <- newKeyPair
    (selfPub, _)  <- newKeyPair
    e             <- newEnclave' [kp1, kp2]
    let crypt = Crypt
            { encryptPayload = enclaveEncryptPayload e
            , decryptPayload = enclaveDecryptPayload e
            }
    storage <- directoryStorage $ d </> "storage"
    port    <- getUnusedPort
    m       <- setupClientTls
        (d </> "tls-client-cert.pem") [] (d </> "tls-client-key.pem")
        (d </> "tls-knownservers") ctrust "localhost"
    nvar    <- newTVarIO =<<
        newNode crypt storage (tformat "https://localhost:{}/" [port])
        [pub1, pub2] [] selfPub [] m True
    tlsSettings <- setupServerTls
        (d </> "tls-server-cert.pem") [] (d </> "tls-server-key.pem")
        (d </> "tls-knownclients") strust "localhost"
    return (nvar, port, tlsSettings)

link :: TVar Node -> TVar Node -> STM ()
link nvar onvar = readTVar onvar >>= \onode ->
    addParty (piUrl $ nodePi onode) nvar

testSendPayload :: TVar Node -> TVar Node -> Assertion
testSendPayload nvar onvar = do
    [node, onode] <- atomically $ mapM readTVar [nvar, onvar]
    let pl   = "foo bar baz"
        from = firstPublicKey node
        to   = firstPublicKey onode
    es <- sendPayload node pl (Just from) [to]
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
kvTest name tests f = testGroup (trunc name) $ map gen tests
  where
    gen (input, output) = testCase (trunc $ show input) $ f input @?= output
    trunc               = truncateString 50
