{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.Directory.Test where

import ClassyPrelude
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

import Constellation.Node.Storage.Directory (directoryStorage)
import Constellation.Node.Storage.TestUtil (testStorage)

tests :: TestTree
tests = testGroup "Node.Storage.Directory"
    [ testDirectory
    ]

testDirectory :: TestTree
testDirectory = testCaseSteps "storage" $ \step ->
    withSystemTempDirectory "constellation-test-XXX" $ \tempDir -> do
        step "Setting up Directory instance"
        storage <- directoryStorage tempDir
        testStorage storage "testDirectory" step        
