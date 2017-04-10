{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Either.Test where

import ClassyPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Constellation.Util.Either (fromRight)

tests :: TestTree
tests = testGroup "Util.Either"
    [ testFromEither
    ]

testFromEither :: TestTree
testFromEither = testProperty "fromRight"
    (\x -> fromRight (Right (x :: Integer)) == x)
