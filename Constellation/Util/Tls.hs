{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Tls where

import ClassyPrelude
import Crypto.Hash.Algorithms (SHA512(..))
import Crypto.PubKey.RSA
    (PublicKey(..), PrivateKey(..), Blinder, generate, generateBlinder)
import Crypto.PubKey.RSA.PKCS15
import Crypto.Random (MonadRandom, seedNew, seedToInteger)
import Data.ASN1.BinaryEncoding (DER(..))
import Data.ASN1.Encoding (encodeASN1')
import Data.ASN1.Types
    (ASN1(..), ASN1ConstructionType(..), ASN1Object(..), getObjectID)
import Data.PEM (PEM(..), pemWriteBS)
import Data.X509
    ( Certificate(..), PubKey(..), PrivKey(..)
    , SignatureALG(..), HashALG(..), SignedExact, DistinguishedName(..)
    , DnElement(..), Extensions(..), ExtKeyUsage(..), ExtKeyUsageFlag(..)
    , ASN1CharacterString
    , extensionEncode, pubkeyToAlg, privkeyToAlg, encodeSignedObject
    , objectToSignedExact
    )

import Time.System (dateCurrent)
import Time.Types (Date(..), DateTime(..))
import qualified Data.Text as T

import Constellation.Util.Either (fromShowRight)
import Constellation.Util.File (withOwnerReadWrite, writeFileBs)

maxSerialNumber :: Integer
maxSerialNumber = 1461501637330902918203684832716283019655932542975  -- 2^(8*20)-1

newSelfSignedCertificate :: FilePath -> FilePath -> Text -> IO ()
newSelfSignedCertificate certPath keyPath cn = do
    today    <- dateCurrent
    (cb, kb) <- newCertAndKeyBytes today (fromString $ T.unpack cn)
    writeFileBs certPath cb
    withOwnerReadWrite $ writeFileBs keyPath kb

newCertAndKeyBytes :: MonadRandom m
                   => DateTime
                   -> ASN1CharacterString
                   -> m (ByteString, ByteString)
newCertAndKeyBytes today cn = newCertAndKey today cn
    >>= \(cert, key) -> return ( pemWriteBS $ certPem cert
                               , pemWriteBS $ keyPem key
                               )

newCertAndKey :: MonadRandom m
              => DateTime
              -> ASN1CharacterString
              -> m (SignedExact Certificate, PrivateKey)
newCertAndKey today cn = do
    (pub, priv) <- newRsa
    blinder     <- generateBlinder $ public_n pub
    serial      <- newSerial
    let cert        = x509Certificate (PubKeyRSA pub) serial today cn
        (signed, _) = objectToSignedExact
            (signWithSha512 blinder (PrivKeyRSA priv))
            cert
    return (signed, priv)

-- | Generates a 4096-bit RSA key pair using the 65537 public exponent
newRsa :: MonadRandom m => m (PublicKey, PrivateKey)
newRsa = generate (4096 `div` 8) 65537

-- | Generates a random X509 serial number
newSerial :: MonadRandom m => m Integer
newSerial = fmap (`mod` maxSerialNumber) (seedToInteger <$> seedNew)

-- | Generates an X509 certificate valid from one year ago to ten years in
-- the future with the specified Common Name (CN). The certificate authorizes
-- only signing.
x509Certificate :: PubKey
                -> Integer
                -> DateTime
                -> ASN1CharacterString
                -> Certificate
x509Certificate pub serial today cn = Certificate
    { certVersion      = 3
    , certSerial       = serial
    , certSignatureAlg = SignatureALG HashSHA512 (pubkeyToAlg pub)
    , certIssuerDN     = dn "constellation-node"
    , certValidity     = (past1Year, future10Years)
    , certSubjectDN    = dn cn
    , certPubKey       = pub
    , certExtensions   = Extensions $ Just
        [extensionEncode True $ ExtKeyUsage [KeyUsage_digitalSignature]]
    }
  where
    past1Year     = today { dtDate = curDate { dateYear = curYear - 1 } }
    future10Years = today { dtDate = curDate { dateYear = curYear + 10 } }
    curDate       = dtDate today
    curYear       = dateYear curDate

dn :: ASN1CharacterString -> DistinguishedName
dn s = DistinguishedName
    [ (getObjectID DnCommonName, s)
    , (getObjectID DnCountry, "US")
    , (getObjectID DnOrganization, "constellation-node")
    , (getObjectID DnOrganizationUnit, "tls")
    ]

signWithSha512 :: Blinder
               -> PrivKey
               -> ByteString
               -> (ByteString, SignatureALG, ())
signWithSha512 blinder priv b =
    (sig, SignatureALG HashSHA512 (privkeyToAlg priv), ())
  where
    sig = case priv of
        PrivKeyRSA p -> fromShowRight $ sign (Just blinder) (Just SHA512) p b
        _            -> error "signWithSha512: Unsupported public key algorithm"

newtype Asn1PrivateKey = Asn1PrivateKey { unAsn1PrivateKey :: PrivateKey }

instance ASN1Object Asn1PrivateKey where
    toASN1 (Asn1PrivateKey PrivateKey{..}) xs =
        Start Sequence
        : IntVal 0  -- Version
        : IntVal (public_n private_pub)
        : IntVal (public_e private_pub)
        : IntVal private_d
        : IntVal private_p
        : IntVal private_q
        : IntVal private_dP
        : IntVal private_dQ
        : IntVal private_qinv
        : End Sequence
        : xs
    fromASN1                                  =
        error "fromASN1 PrivateKey not implemented"

certPem :: SignedExact Certificate -> PEM
certPem cert = PEM
    { pemName    = "CERTIFICATE"
    , pemHeader  = []
    , pemContent = encodeSignedObject cert
    }

keyPem :: PrivateKey -> PEM
keyPem key = PEM
    { pemName    = "RSA PRIVATE KEY"
    , pemHeader  = []
    , pemContent = asn1ObjectToDerBytes (Asn1PrivateKey key)
    }

asn1ObjectToDerBytes :: ASN1Object a => a -> ByteString
asn1ObjectToDerBytes obj = encodeASN1' DER $ toASN1 obj []
