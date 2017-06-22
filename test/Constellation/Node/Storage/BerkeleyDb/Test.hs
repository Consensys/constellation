{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.BerkeleyDb.Test where

import ClassyPrelude
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

import Constellation.Node.Storage.BerkeleyDb (berkeleyDbStorage)
import Constellation.Node.Storage.TestUtil (testStorage)

tests :: TestTree
tests = testGroup "Node.Storage.BerkeleyDb"
    [ testBerkeleyDb
    ]

testBerkeleyDb :: TestTree
testBerkeleyDb = testCaseSteps "storage" $ \step ->
    withSystemTempDirectory "constellation-test-XXX" $ \tempDir-> do
        step "Setting up BerkeleyDb instance"
        storage <- berkeleyDbStorage tempDir
        testStorage storage "testBerkeleyDb" step
