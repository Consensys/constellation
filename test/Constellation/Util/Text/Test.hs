{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Text.Test where

import ClassyPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Constellation.Util.Text (tformat)

tests :: TestTree
tests = testGroup "Util.Text"
    [ testTformat
    ]

testTformat :: TestTree
testTformat = testCase "tformat" $ tformat
    "There is {} apple in the {}." ["1" :: Text, "garden"] @?=
    "There is 1 apple in the garden."
