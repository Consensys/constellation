{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Trust.Test where

import ClassyPrelude hiding (encodeUtf8)
import Data.Default (def)
import Data.X509 (CertificateChain(..), getCertificate)
import Data.X509.Validation (ValidationCacheResult(..))
import Network.TLS (ValidationCache(..), credentialLoadX509ChainFromMemory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, (@?=), testCaseSteps)

import Constellation.Node.Main (setupClientTls, setupServerTls)
import Constellation.Node.Trust
    (TrustMode(..), KnownHosts(..), validationCache, saveKnownHosts')
import Constellation.Util.Either (fromShowRight)

import Constellation.MockData
import Constellation.TestUtil (kvTest, withThreeTestNodes, testSendPayload)

tests :: TestTree
tests = testGroup "Constellation.Node.Trust"
    [ testTrustModes
      -- TODO: Test for actual Warp and Manager
    ]

testTrustModes :: TestTree
testTrustModes = testCaseSteps "testTrustModes" $ \step -> do
    let (CertificateChain (ssigned1:_), _) = fromShowRight $
            credentialLoadX509ChainFromMemory testingOnlyMockServerCert1 []
            testingOnlyMockServerKey1
        (CertificateChain (ssigned2:_), _) = fromShowRight $
            credentialLoadX509ChainFromMemory testingOnlyMockServerCert2 []
            testingOnlyMockServerKey2
        (CertificateChain (ssigned3:_), _) = fromShowRight $
            credentialLoadX509ChainFromMemory testingOnlyMockServerCert3 []
            testingOnlyMockServerKey3
        (CertificateChain (csigned1:_), _) = fromShowRight $
            credentialLoadX509ChainFromMemory testingOnlyMockClientCert1 []
            testingOnlyMockClientKey1
        (CertificateChain (csigned2:_), _) = fromShowRight $
            credentialLoadX509ChainFromMemory testingOnlyMockClientCert2 []
            testingOnlyMockClientKey2
        (CertificateChain (csigned3:_), _) = fromShowRight $
            credentialLoadX509ChainFromMemory testingOnlyMockClientCert3 []
            testingOnlyMockClientKey3
        scert1 = getCertificate ssigned1
        scert2 = getCertificate ssigned2
        scert3 = getCertificate ssigned3
        ccert1 = getCertificate csigned1
        ccert2 = getCertificate csigned2
        ccert3 = getCertificate csigned3

    withTestVc def Whitelist $ \ValidationCache{..} -> do
        -- This should fail because knownhosts is empty and whitelist doesn't
        -- add anything
        step "Whitelist - Fail to add unknown fingerprint"
        cacheQuery ("foo.bar.baz.com", "") testingOnlyMockServerFingerprint1 scert1
            >>= \res -> res @?= ValidationCacheDenied "Failed trust validation"

    withTestVc def Tofu $ \ValidationCache{..} -> do
        step "Tofu - Add first fingerprint with empty knownhosts"
        -- This should succeed because knownhosts is empty
        cacheQuery ("foo.bar.baz.com", "") testingOnlyMockServerFingerprint1 scert1
            >>= \res -> res @?= ValidationCachePass
        step "Tofu - Fail to add second fingerprint"
        -- This should fail because only fingerprint 1 is trusted for host 1
        cacheQuery ("foo.bar.baz.com", "") testingOnlyMockServerFingerprint2 scert2
            >>= \res -> res @?= ValidationCacheDenied "Failed trust validation"
        step "Tofu - Verify that the first fingerprint still works"
        -- Fingerprint 1 should still succeed
        cacheQuery ("foo.bar.baz.com", "") testingOnlyMockServerFingerprint1 scert1
            >>= \res -> res @?= ValidationCachePass
        step "Tofu - Adding another host should still work"
        -- Another host should still work. Using client fingerprint only
        -- because those testing certificates have a different hostname.
        cacheQuery ("127.0.0.1", "") testingOnlyMockClientFingerprint1 ccert1
            >>= \res -> res @?= ValidationCachePass

    -- TODO: All modes

withTestVc :: KnownHosts
           -> TrustMode
           -> (ValidationCache -> Assertion)
           -> Assertion
withTestVc kh t f = withSystemTempDirectory "c11n-trust-" $ \fpath -> do
    let khPath = fpath </> "tls-known-hosts"
    saveKnownHosts' khPath kh
    validationCache khPath t >>= f
