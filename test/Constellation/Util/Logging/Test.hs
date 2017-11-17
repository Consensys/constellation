{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Logging.Test where

import ClassyPrelude
import Test.Tasty (TestTree, testGroup)

import Constellation.Util.Text (tformat)

tests :: TestTree
tests = testGroup "Util.Logging" []
