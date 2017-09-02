{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.LevelDb.Test where

import ClassyPrelude
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

import Constellation.Node.Storage.LevelDb (levelDbStorage)
import Constellation.Node.Storage.TestUtil (testStorage)

tests :: TestTree
tests = testGroup "Node.Storage.LevelDb"
    [ testLevelDb
    ]

testLevelDb :: TestTree
testLevelDb = testCaseSteps "storage" $ \step ->
    withSystemTempDirectory "constellation-test-XXX" $ \tempDir-> do
        step "Setting up LevelDB instance"
        storage <- levelDbStorage tempDir
        testStorage storage "testLevelDb" step
