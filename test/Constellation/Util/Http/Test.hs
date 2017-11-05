{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Http.Test where

import ClassyPrelude
import Network.HTTP.Types (Header, RequestHeaders)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Data.ByteString.Char8 as BC

import Constellation.Util.Http (getHeaderValues, getHeaderCommaValues)

tests :: TestTree
tests = testGroup "Util.Http"
    [ testGetHeaderValues
    , testGetHeaderValuesInvalid
    , testGetHeaderCommaValues
    ]

h1 :: Header
h1 = ("h1", BC.pack "value1")

h2 :: Header
h2 = ("h2", BC.pack "value2,value3")

h2Plus :: Header
h2Plus = ("h2", BC.pack "value4")

hs :: RequestHeaders
hs = [h1, h2, h2Plus]

testGetHeaderValues :: TestTree
testGetHeaderValues = testCase "getHeaderValues" $
    getHeaderValues "h1" hs @?= [snd h1]

testGetHeaderValuesInvalid :: TestTree
testGetHeaderValuesInvalid = testCase "getHeaderValuesInvalid" $
    getHeaderValues "invalid" hs @?= []

testGetHeaderCommaValues :: TestTree
testGetHeaderCommaValues = testCase "getHeaderCommaValues" $
    getHeaderCommaValues "h2" hs @?= ["value2", "value3", "value4"]
