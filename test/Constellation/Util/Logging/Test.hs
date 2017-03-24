{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Constellation.Util.Logging.Test where

import ClassyPrelude
import Test.Tasty (TestTree)

import Constellation.Util.Text (tformat)

tests :: TestTree
tests = testGroup "Util.Logging" []
