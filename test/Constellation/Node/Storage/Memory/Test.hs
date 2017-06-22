{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.Memory.Test where

import ClassyPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

import Constellation.Node.Storage.Memory (memoryStorage)
import Constellation.Node.Storage.TestUtil (testStorage)

tests :: TestTree
tests = testGroup "Node.Storage.Memory"
    [ testMemory
    ]

testMemory :: TestTree
testMemory = testCaseSteps "storage" $ \step -> do
    step "Setting up Memory instance"
    storage <- memoryStorage
    testStorage storage "testMemory" step        
