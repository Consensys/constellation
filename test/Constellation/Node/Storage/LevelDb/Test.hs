{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.LevelDb.Test where

import ClassyPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

import Constellation.Node.Storage.LevelDb (levelDbStorage)
import Constellation.Node.Storage.TestUtil (testStorage)

tests :: TestTree
tests = testGroup "Node.Storage.LevelDb"
    [ testLevelDb
    ]

testLevelDb :: TestTree
testLevelDb = testCaseSteps "storage" $ \step -> do
    step "Setting up LevelDb instance"
    storage <- levelDbStorage "constellation-test-leveldb"
    testStorage storage "testLevelDb" step
