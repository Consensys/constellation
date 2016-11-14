{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Constellation.Node.Api.Test where

import ClassyPrelude
import Control.Concurrent (forkIO)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import qualified Network.Wai.Handler.Warp as Warp

import Constellation.Node (nodeRefresh)
import Constellation.Node.Types (Node(nodeStorage), Storage(closeStorage))
import qualified Constellation.Node.Api as NodeApi

import Constellation.TestUtil (setupTestNode, link, testSendPayload)

tests :: TestTree
tests = testGroup "Constellation.Node.Api"
    [ testSendAndReceivePayload
    ]

testSendAndReceivePayload :: TestTree
testSendAndReceivePayload = testCaseSteps "Testing sending and receiving of a payload" $ \step ->
    withSystemTempDirectory "constellation-test-XXX" $ \d -> do
        step "Setting up nodes"
        (node1Var, port1) <- setupTestNode d "node1"
        (node2Var, port2) <- setupTestNode d "node2"
        (node3Var, port3) <- setupTestNode d "node3"
        tid1 <- forkIO $ Warp.run port1 $ NodeApi.app node1Var
        tid2 <- forkIO $ Warp.run port2 $ NodeApi.app node2Var
        tid3 <- forkIO $ Warp.run port3 $ NodeApi.app node3Var

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
