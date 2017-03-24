{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Constellation.Node.Api.Test where

import ClassyPrelude
import Control.Concurrent (forkIO)
import Data.IP (toHostAddress, toHostAddress6)
import Network.Socket
    (SockAddr(SockAddrInet, SockAddrInet6, SockAddrUnix, SockAddrCan))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase, testCaseSteps)
import Text.Read (read)
import qualified Network.Wai.Handler.Warp as Warp

import Constellation.Node (nodeRefresh)
import Constellation.Node.Types (Node(nodeStorage), Storage(closeStorage))
import qualified Constellation.Node.Api as NodeApi

import Constellation.TestUtil (setupTestNode, link, testSendPayload)

tests :: TestTree
tests = testGroup "Constellation.Node.Api"
    [ testWhitelist
    , testSendAndReceivePayload
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
        tid1 <- forkIO $ Warp.run port1 $ NodeApi.app Nothing True node1Var
        tid2 <- forkIO $ Warp.run port2 $ NodeApi.app Nothing True node2Var
        tid3 <- forkIO $ Warp.run port3 $ NodeApi.app Nothing True node3Var

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
