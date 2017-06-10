{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Storage.Sqlite.Test where

import ClassyPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

import Constellation.Node.Storage.Sqlite (sqliteStorage)
import Constellation.Node.Storage.TestUtil (testStorage)

tests :: TestTree
tests = testGroup "Node.Storage.Sqlite"
    [ testSqlite
    ]

testSqlite :: TestTree
testSqlite = testCaseSteps "storage" $ \step -> do
    step "Setting up sqlite instance"
    storage <- sqliteStorage "constellation-test-sqlite"
    testStorage storage "testSqlite" step
