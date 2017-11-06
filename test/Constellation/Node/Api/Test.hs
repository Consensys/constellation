{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Api.Test where

import ClassyPrelude hiding (encodeUtf8)
import Control.Concurrent (forkIO)
import Data.Maybe (fromJust)
import Data.IP (toHostAddress, toHostAddress6)
import Data.Text.Encoding (encodeUtf8)
import Network.Socket (SockAddr(SockAddrInet, SockAddrInet6, SockAddrUnix, SockAddrCan))
import Network.HTTP.Types.Header (hContentLength)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase, testCaseSteps)
import Text.Read (read)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Network.Wai.Handler.Warp as Warp

import Constellation.Enclave.Types (PublicKey, mkPublicKey)
import Constellation.Node (nodeRefresh)
import Constellation.Node.Api (ApiType(..), Send(..))
import Constellation.Node.Types (Node(nodeStorage), Storage(closeStorage))
import Constellation.Util.ByteString (mustB64TextDecodeBs)
import qualified Constellation.Node.Api as NodeApi

import Constellation.TestUtil (kvTest, setupTestNode, link, testSendPayload)

tests :: TestTree
tests = testGroup "Constellation.Node.Api"
    [ testWhitelist
    , testSendAndReceivePayload
    , testMustDecodeB64PublicKey
    , testDecodeSendRaw
    ]

testWhitelist :: TestTree
testWhitelist = testCase "whitelist" $ do
    let wled = NodeApi.whitelisted wl
        wl   = NodeApi.whitelist
            [ "10.0.0.1"
            , "2001:0db8:85a3:0000:0000:8a2e:0370:7334"
            ]
    wled (SockAddrInet 0 $ toHostAddress $ read "10.0.0.1") @?= True
    wled (SockAddrInet 0 $ toHostAddress $ read "10.0.0.2") @?= False
    wled (SockAddrInet6 0 0 (toHostAddress6 $ read "2001:0db8:85a3:0000:0000:8a2e:0370:7334") 0) @?= True
    wled (SockAddrInet6 0 0 (toHostAddress6 $ read "2001:0db8:85a3:0000:0000:8a2e:0370:7335") 0) @?= False
    wled (SockAddrUnix "foo") @?= False
    wled (SockAddrCan 42) @?= False

testSendAndReceivePayload :: TestTree
testSendAndReceivePayload = testCaseSteps "sendAndReceivePayload" $ \step ->
    withSystemTempDirectory "constellation-test-XXX" $ \d -> do
        step "Setting up nodes"
        (node1Var, port1) <- setupTestNode d "node1"
        (node2Var, port2) <- setupTestNode d "node2"
        (node3Var, port3) <- setupTestNode d "node3"
        tid1 <- forkIO $ Warp.run port1 $ NodeApi.app Nothing Private node1Var
        tid2 <- forkIO $ Warp.run port2 $ NodeApi.app Nothing Private node2Var
        tid3 <- forkIO $ Warp.run port3 $ NodeApi.app Nothing Private node3Var

        step "Linking nodes"
        atomically $ do
            link node1Var node2Var
            link node2Var node3Var

        let nvars = [node1Var, node2Var, node3Var]
        step "Refreshing nodes"
        mapM_ nodeRefresh nvars  -- Once to discover the first nodes
        mapM_ nodeRefresh nvars  -- Twice to receive updated info
        mapM_ nodeRefresh nvars  -- Thrice to check that nothing is crazy

        -- step "Ensuring the nodes form a network"
        -- TODO
        -- forM_ nvars $ \nvar -> readTVarIO nvar >>= \node ->
        --     putStrLn "" >> print (nodePi node)

        step "Sending a payload from node1 to node2"
        testSendPayload node1Var node2Var
        
        -- step "Ensuring that node3 didn't receive the payload"
        -- TODO
        
        step "Cleaning up"
        killThread tid1
        killThread tid2
        killThread tid3
        nodes <- atomically $ mapM readTVar [node1Var, node2Var, node3Var]
        mapM_ (closeStorage . nodeStorage) nodes

pl :: String
pl = "payload"

pll :: ByteString
pll = (BC.pack . show . length) pl

pk1t :: Text
pk1t = "BULeR8JyUWhiuuCMU/HLA0Q5pzkYT+cHII3ZKBey3Bo="

pk1bs :: ByteString
pk1bs = encodeUtf8 pk1t

pk1 :: PublicKey
pk1 = mustB64TextMkPublicKey pk1t

pk2t :: Text
pk2t = "QfeDAys9MPDs2XHExtc84jKGHxZg/aj52DTh0vtA3Xc="

pk2bs :: ByteString
pk2bs = encodeUtf8 pk2t

pk2 :: PublicKey
pk2 = mustB64TextMkPublicKey pk2t

mustB64TextMkPublicKey :: Text -> PublicKey
mustB64TextMkPublicKey = fromJust . mkPublicKey . mustB64TextDecodeBs

testMustDecodeB64PublicKey :: TestTree
testMustDecodeB64PublicKey = kvTest "mustDecodeB64PublicKey"
    [(pk1bs, pk1)]
    NodeApi.mustDecodeB64PublicKey

testDecodeSendRaw :: TestTree
testDecodeSendRaw = testCase "decodeSendRaw" $
    NodeApi.decodeSendRaw
         (BLC.pack pl)
         [(hContentLength, pll), (NodeApi.hFrom, pk1bs), (NodeApi.hTo, pk2bs)]
    @?=
    Right NodeApi.Send
        { sreqPayload = BC.pack pl
        , sreqFrom    = pk1
        , sreqTo      = [pk2]
        }
