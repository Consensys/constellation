{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Constellation.Util.Lockable.Test where

import ClassyPrelude
import Crypto.KDF.Argon2 (Options(..), Variant(..), Version(..))
import Data.Aeson (encode, decode)
import Data.Tuple (swap)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Constellation.TestUtil (kvTest)
import Constellation.Util.Lockable
    (ArgonOptions (ArgonOptions), defaultArgonOptions)

tests :: TestTree
tests = testGroup "Util.Lockable"
    [ testToJsonFromArgonOptions
    , testFromJsonToArgonOptions
    , testFromJsonToArgonOptionsNoVersion
    ]

argonOptionsKvs :: [(ArgonOptions, Text)]
argonOptionsKvs =
    [ ( defaultArgonOptions
      , "{\"variant\":\"i\",\"memory\":1048576,\"iterations\":10,\"parallelism\":4,\"version\":\"1.3\"}"
      )
    , ( ArgonOptions Options
        { iterations  = 1
        , memory      = (2 :: Word32) ^ (10 :: Word32)  -- 1 KB
        , parallelism = 2
        , variant     = Argon2id
        , version     = Version10
        }
      , "{\"variant\":\"id\",\"memory\":1024,\"iterations\":1,\"parallelism\":2,\"version\":\"1.0\"}"
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
    decode "{\"variant\":\"i\",\"memory\":1048576,\"iterations\":10,\"parallelism\":4}"
    @?= (Just defaultArgonOptions)
    
justKvsSwap :: (a, b) -> (b, Maybe a)
justKvsSwap (a, b) = (b, Just a)
