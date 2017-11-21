{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Api.Test where

import ClassyPrelude hiding (encodeUtf8)
import Data.Maybe (fromJust)
import Data.IP (toHostAddress, toHostAddress6)
import Data.Text.Encoding (encodeUtf8)
import Network.Socket (SockAddr(SockAddrInet, SockAddrInet6, SockAddrUnix, SockAddrCan))
import Network.HTTP.Types.Header (hContentLength)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.Read (read)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC

import Constellation.Enclave.Types (PublicKey, mkPublicKey)
import Constellation.Node.Api
    ( Send(..), hFrom, hTo, whitelist, whitelisted, decodeSendRaw
    , mustDecodeB64PublicKey
    )
import Constellation.Util.ByteString (mustB64TextDecodeBs)

import Constellation.TestUtil (kvTest, withThreeTestNodes, testSendPayload)

tests :: TestTree
tests = testGroup "Constellation.Node.Api"
    [ testWhitelist
    , testSendAndReceivePayload
    , testMustDecodeB64PublicKey
    , testDecodeSendRaw
    ]

testWhitelist :: TestTree
testWhitelist = testCase "whitelist" $ do
    let wled = whitelisted wl
        wl   = whitelist
            [ "10.0.0.1"
            , "2001:0db8:85a3:0000:0000:8a2e:0370:7334"
            ]
    wled (SockAddrInet 0 $ toHostAddress $ read "10.0.0.1") @?= True
    wled (SockAddrInet 0 $ toHostAddress $ read "10.0.0.2") @?= False
    wled (SockAddrInet6 0 0 (toHostAddress6 $ read "2001:0db8:85a3:0000:0000:8a2e:0370:7334") 0) @?= True
    wled (SockAddrInet6 0 0 (toHostAddress6 $ read "2001:0db8:85a3:0000:0000:8a2e:0370:7335") 0) @?= False
    wled (SockAddrUnix "foo") @?= False
    wled (SockAddrCan 42) @?= False

testSendAndReceivePayload :: TestTree
testSendAndReceivePayload = testCase "sendAndReceivePayload" $
    withThreeTestNodes $ \(nvar1, nvar2, nvar3) -> do
        -- TODO Ensure the nodes form a network
        testSendPayload nvar1 nvar2
        -- TODO "Ensure that node3 didn't receive the payload"

pl :: String
pl = "payload"

pll :: ByteString
pll = (BC.pack . show . length) pl

pk1t :: Text
pk1t = "BULeR8JyUWhiuuCMU/HLA0Q5pzkYT+cHII3ZKBey3Bo="

pk1bs :: ByteString
pk1bs = encodeUtf8 pk1t

pk1 :: PublicKey
pk1 = mustB64TextMkPublicKey pk1t

pk2t :: Text
pk2t = "QfeDAys9MPDs2XHExtc84jKGHxZg/aj52DTh0vtA3Xc="

pk2bs :: ByteString
pk2bs = encodeUtf8 pk2t

pk2 :: PublicKey
pk2 = mustB64TextMkPublicKey pk2t

mustB64TextMkPublicKey :: Text -> PublicKey
mustB64TextMkPublicKey = fromJust . mkPublicKey . mustB64TextDecodeBs

testMustDecodeB64PublicKey :: TestTree
testMustDecodeB64PublicKey = kvTest "mustDecodeB64PublicKey"
    [(pk1bs, pk1)]
    mustDecodeB64PublicKey

testDecodeSendRaw :: TestTree
testDecodeSendRaw = testCase "decodeSendRaw" $
    decodeSendRaw
         (BLC.pack pl)
         [(hContentLength, pll), (hFrom, pk1bs), (hTo, pk2bs)]
    @?=
    Right Send
        { sreqPayload = BC.pack pl
        , sreqFrom    = Just pk1
        , sreqTo      = [pk2]
        }
