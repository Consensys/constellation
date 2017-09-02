{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.Sqlite.Test where

import ClassyPrelude
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

import Constellation.Node.Storage.Sqlite (sqliteStorage)
import Constellation.Node.Storage.TestUtil (testStorage)

tests :: TestTree
tests = testGroup "Node.Storage.Sqlite"
    [ testSqlite
    ]

testSqlite :: TestTree
testSqlite = testCaseSteps "storage" $ \step ->
    withSystemTempDirectory "constellation-test-XXX" $ \tempDir-> do
        step "Setting up SQLite instance"
        storage <- sqliteStorage (tempDir </> "test.db")
        testStorage storage "testSqlite" step
