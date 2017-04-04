{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.AtExit.Test where

import ClassyPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Constellation.Util.AtExit (registerAtExit, withAtExit)

tests :: TestTree
tests = testGroup "Util.AtExit"
    [ testAtExit
    ]

testAtExit :: TestTree
testAtExit = testCase "testAtExit" $ do
    r <- newIORef False
    registerAtExit $ writeIORef r True
    withAtExit $ return ()
    cur <- readIORef r
    cur @?= True
