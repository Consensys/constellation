{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Constellation.Util.String.Test where

import ClassyPrelude
import Data.Char (isSpace)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Constellation.Util.String (trimLeft, trimRight, trimBoth)

tests :: TestTree
tests = testGroup "Util.String"
    [ testTrimLeft
    , testTrimRight
    , testTrimBoth
    ]

testTrimLeft :: TestTree
testTrimLeft = testProperty "trimLeft"
    (\x -> null $ takeWhile isSpace $ trimLeft x)

testTrimRight :: TestTree
testTrimRight = testProperty "trimRight"
    (\x -> null $ takeWhile isSpace $ reverse $ trimRight x)

testTrimBoth :: TestTree
testTrimBoth = testProperty "trimBoth"
    (\x -> let s = trimBoth x
            in null (takeWhile isSpace s) &&
               null (takeWhile isSpace $ reverse s)
    )
