{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Exception.Test where

import ClassyPrelude hiding (assert)
import Test.QuickCheck.Monadic (PropertyM, monadicIO, run, assert)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import qualified Control.Exception as E

import Constellation.Util.Exception (trys, someExceptionToStringEither)

data MockException = MockException String
                   deriving Show

instance Exception MockException

tests :: TestTree
tests = testGroup "Util.Exception"
    [ testTrys
    , testSomeExceptionToStringEither
    ]

testTrys :: TestTree
testTrys = testProperty "trys" $ \s -> monadicIO $ do
    ee <- run $ trys (throw $ MockException s)
    trysAssert ee s

trysAssert :: Monad m => Either String () -> String -> PropertyM m ()
trysAssert ee s = assert $ ee == Left ("MockException " ++ show s)

testSomeExceptionToStringEither :: TestTree
testSomeExceptionToStringEither = testProperty "someExceptionToStringEither" $ \s -> monadicIO $ do
    ee <- run $ E.try (throw $ MockException s)
    trysAssert (someExceptionToStringEither ee) s
