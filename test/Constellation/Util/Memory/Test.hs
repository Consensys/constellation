{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Constellation.Util.Memory.Test where

import ClassyPrelude hiding (assert)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B

import Constellation.Util.Memory (byteArrayToByteString)

tests :: TestTree
tests = testGroup "Util.Memory"
    [ testByteArrayToByteString
    ]

testByteArrayToByteString :: TestTree
testByteArrayToByteString = testProperty "byteArrayToByteString" $ \ws ->
    B.unpack (byteArrayToByteString (BA.pack ws :: BA.Bytes)) == ws
