{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Lockable.Test where

import ClassyPrelude
import Crypto.KDF.Argon2 (Options(..), Variant(..), Version(..))
import Data.Aeson (encode, decode)
import Data.Maybe (fromJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase, assertFailure)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Constellation.TestUtil (kvTest)
import Constellation.Util.Lockable
    (ArgonOptions (ArgonOptions), defaultArgonOptions, unlock)

tests :: TestTree
tests = testGroup "Util.Lockable"
    [ testToJsonFromArgonOptions
    , testFromJsonToArgonOptions
    , testFromJsonToArgonOptionsNoVersion
    , testUnlockV0
    ]

argonOptionsKvs :: [(ArgonOptions, Text)]
argonOptionsKvs =
    [ ( defaultArgonOptions
      , "{\"variant\":\"id\",\"memory\":1048576,\"iterations\":10,\"parallelism\":4,\"version\":\"1.3\"}"
      )
    , ( ArgonOptions Options
        { iterations  = 1
        , memory      = (2 :: Word32) ^ (10 :: Word32)  -- 1 KB
        , parallelism = 2
        , variant     = Argon2d
        , version     = Version10
        }
      , "{\"variant\":\"d\",\"memory\":1024,\"iterations\":1,\"parallelism\":2,\"version\":\"1.0\"}"
      )
    ]

testToJsonFromArgonOptions :: TestTree
testToJsonFromArgonOptions = kvTest "toJsonFromArgonOptions"
    argonOptionsKvs $ TL.toStrict . TLE.decodeUtf8 . encode
  
testFromJsonToArgonOptions :: TestTree
testFromJsonToArgonOptions = kvTest "fromJsonToArgonOptions"
    (map justKvsSwap argonOptionsKvs) $ decode . TLE.encodeUtf8 . TL.fromStrict

testFromJsonToArgonOptionsNoVersion :: TestTree
testFromJsonToArgonOptionsNoVersion = testCase "testFromJsonToArgonOptionsNoVersion" $
    decode "{\"variant\":\"id\",\"memory\":1048576,\"iterations\":10,\"parallelism\":4}"
    @?= Just defaultArgonOptions
    
justKvsSwap :: (a, b) -> (b, Maybe a)
justKvsSwap (a, b) = (b, Just a)

testUnlockV0 :: TestTree
testUnlockV0 = testCase "testUnlockV0" $
    case unlock "foo" $ fromJust $ decode oldKey of
        Right _ -> return ()
        Left _  -> assertFailure "Unlock failed"
  where
    oldKey = "{\"data\":{\"aopts\":{\"variant\":\"i\",\"memory\":8192,\"iterations\":2,\"parallelism\":2},\"snonce\":\"LJ7QUiZ0kD/mPf/aHvCEQKDhlUucZM0s\",\"asalt\":\"I5bvdFhNpr8hlknzOHIO7Q3S5Ubs9ucUdVneghrebZQ=\",\"sbox\":\"iHJAfcpWWE5PsvwZRwr5gCtwMKu7xXZAAPCVRGfx06l/I5QgXs0V/czOqYsMxoEG\"},\"type\":\"argon2sbox\"}"
