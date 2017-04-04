{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.ByteString.Test where

import ClassyPrelude
import Test.Tasty (TestTree, testGroup)
import qualified Data.Text.Encoding as TE

import Constellation.TestUtil (kvTest)
import Constellation.Util.ByteString (mustB64DecodeBs, mustB64TextDecodeBs)

tests :: TestTree
tests = testGroup "Util.ByteString"
    [ testMustB64DecodeBs
    , testMustB64TextDecodeBs
    ]

b64Kvs :: [(Text, Text)]
b64Kvs =
    [ ( "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQh"
      , "Lorem ipsum dolor sit amet!"
      )
    , ( "U2VkIGF1Z3VlIGVyb3MsIGdyYXZpZGEgc2VkLg=="
      , "Sed augue eros, gravida sed."
      )
    , ( "QWVuZWFuIGVnZXQgbGVvIG5vbiBhdWd1ZSBjb252YWxsaXMuLi4="
      , "Aenean eget leo non augue convallis..."
      )
    ]

testMustB64DecodeBs :: TestTree
testMustB64DecodeBs = kvTest "mustDecodeBase64ByteString"
    b64Kvs $ TE.decodeUtf8 . mustB64DecodeBs . TE.encodeUtf8

testMustB64TextDecodeBs :: TestTree
testMustB64TextDecodeBs = kvTest "utf8Base64TextToByteString"
    b64Kvs $ TE.decodeUtf8 . mustB64TextDecodeBs
