{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Trust where

import ClassyPrelude hiding (lookup)
import Prelude (lookup)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.:), (.=), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ASN1.OID (getObjectID)
import Data.ASN1.Types (asn1CharacterToString)
import Data.Default (Default, def)
import Data.Set (Set)
import Data.X509
    ( Certificate(..), CertificateChain(..), HashALG(..), DistinguishedName(..)
    , DnElement(..)
    , getCertificate
    )
import Data.X509.Validation
    ( Fingerprint(..), ValidationCacheQueryCallback, ValidationCacheAddCallback
    , ValidationCacheResult(..)
    , validate, defaultHooks, defaultChecks
    )
import Network.HTTP.Client.TLS (mkManagerSettingsContext)
import Network.HTTP.Conduit (ManagerSettings)
import Network.TLS
    ( ClientParams(..), Cipher(..), Version(..), Shared(..), ClientHooks(..)
    , Supported(..), ValidationCache(..), Credentials(..), Credential
    , ServerHooks(..), CertificateUsage(..), CertificateRejectReason(..)
    , defaultParamsClient
    )
import Network.TLS.Extra.Cipher
    (cipher_ECDHE_RSA_AES128GCM_SHA256, cipher_ECDHE_RSA_AES256GCM_SHA384)
-- import Network.TLS.SessionManager (newSessionManager, defaultConfig)
import Network.Wai.Handler.WarpTLS (TLSSettings(..), tlsSettingsChain)
import System.Directory (doesFileExist, getModificationTime)
import System.X509 (getSystemCertificateStore)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Network.Connection as C

import Constellation.Util.ByteString (hexWithColons)
import Constellation.Util.Either (fromShowRight)
import Constellation.Util.File (writeFileLbs)
import Constellation.Util.Json (HexBs(..), unHexBs)
import Constellation.Util.Logging (warnf)

data TrustMode = Whitelist
               | Tofu
               | Ca
               | CaOrTofu
               | NoValidation
               deriving Eq

instance Show TrustMode where
    show Whitelist    = "whitelist"
    show Tofu         = "tofu"
    show Ca           = "ca"
    show CaOrTofu     = "ca-or-tofu"
    show NoValidation = "insecure-no-validation"

stringToTrustMode :: String -> Maybe TrustMode
stringToTrustMode "whitelist"              = Just Whitelist
stringToTrustMode "tofu"                   = Just Tofu
stringToTrustMode "ca"                     = Just Ca
stringToTrustMode "ca-or-tofu"             = Just CaOrTofu
stringToTrustMode "insecure-no-validation" = Just NoValidation
stringToTrustMode _                        = Nothing

data KnownHosts = KnownHosts
    { khHosts :: HashMap String (Set KnownHost)
    }

instance Default KnownHosts where
    def = KnownHosts
        { khHosts = HM.empty
        }

instance FromJSON KnownHosts where
    parseJSON (Object v) = KnownHosts
        <$> v .: "hosts"
    parseJSON _          = mzero

instance ToJSON KnownHosts where
    toJSON KnownHosts{..} = object
        [ "hosts" .= khHosts
        ]

data KnownHost = RsaSha512Fingerprint ByteString
               deriving (Eq, Ord)

instance Show KnownHost where
    show (RsaSha512Fingerprint b) = show $ hexWithColons b

instance FromJSON KnownHost where
    parseJSON (Object v) = do
        t <- v .: "type"
        case t :: Text of
            "rsa-sha512" -> RsaSha512Fingerprint . unHexBs <$> v .: "data"
            _            -> mzero
    parseJSON _          = mzero

instance ToJSON KnownHost where
    toJSON (RsaSha512Fingerprint b) = object
        [ "type" .= ("rsa-sha512" :: Text)
        , "data" .= HexBs b
        ]

ciphers :: [Cipher]
ciphers =
    [ cipher_ECDHE_RSA_AES128GCM_SHA256
    , cipher_ECDHE_RSA_AES256GCM_SHA384
    -- TODO: cipher_ECDHE-RSA-CHACHA20-POLY1305 if/when added upstream
    -- TODO: Add DHE ciphers enabled by a --insecure-static-dh flag
    -- (for MITM appliances)? Only needs to be on outbound?
    ]

versions :: [Version]
versions = [TLS12]  -- TODO: TLS13 when added upstream

tlsSettings :: FilePath
            -> [FilePath]
            -> FilePath
            -> FilePath
            -> TrustMode
            -> IO TLSSettings
tlsSettings certPath chainPaths keyPath khPath t = do
    vc <- validationCache khPath t
    return $ (tlsSettingsChain certPath chainPaths keyPath)
        { tlsAllowedVersions = versions
        , tlsCiphers         = ciphers
        , tlsWantClientCert  = True
        , tlsServerHooks     = def
            { onClientCertificate = clientCertificateCheck vc
            }
        -- , tlsSessionManagerConfig = Just defaultConfig  -- TODO
        }

clientCertificateCheck :: ValidationCache
                       -> CertificateChain
                       -> IO CertificateUsage
clientCertificateCheck _  (CertificateChain [])          = return $
    CertificateUsageReject $ CertificateRejectOther "No chain"
clientCertificateCheck vc chain@(CertificateChain (c:_)) =
    case certHostname $ getCertificate c of
        Just hostname -> do
            faileds <- validate HashSHA512 defaultHooks defaultChecks mempty vc
                (hostname, "") chain
            return $ if null faileds
                then CertificateUsageAccept
                else CertificateUsageReject $ CertificateRejectOther
                     "Certificate validation failed"
        _             -> return $ CertificateUsageReject $
            CertificateRejectOther "Invalid hostname"

certHostname :: Certificate -> Maybe String
certHostname c = lookup (getObjectID DnCommonName)
    (getDistinguishedElements $ certSubjectDN c)
    >>= asn1CharacterToString

managerSettings :: ValidationCache
                -> Credential
                -> TrustMode
                -> IO ManagerSettings
managerSettings vc cred t = do
    cs  <- if t == Ca || t == CaOrTofu
        then getSystemCertificateStore
        else return mempty
    ctx <- C.initConnectionContext
    let tlsCfg       = C.TLSSettings clientParams
        clientParams = (defaultParamsClient "" "")
            { clientShared    = def
                { sharedCredentials     = Credentials [cred]
                -- , sharedSessionManager  = newSessionManager defaultConfig  -- TODO
                , sharedCAStore         = cs
                , sharedValidationCache = vc
                }
            , clientHooks     = def
                { onCertificateRequest = \_ -> return $ Just cred
                , onServerCertificate  =
                      validate HashSHA512 defaultHooks defaultChecks
                }
            , clientSupported = def
                { supportedVersions = versions
                , supportedCiphers  = ciphers
                }
            }
    return $ mkManagerSettingsContext (Just ctx) tlsCfg Nothing

validationCache :: FilePath -> TrustMode -> IO ValidationCache
validationCache khPath t = do
    khVar <- initKnownHosts khPath
    return $ ValidationCache (vcQuery khPath khVar t) (vcAdd khPath khVar t)

initKnownHosts :: FilePath -> IO (MVar (KnownHosts, UTCTime))
initKnownHosts khPath = do
    exists <- doesFileExist khPath
    when (not exists) $ saveKnownHosts def khPath
    mtime <- getModificationTime khPath
    kh    <- fromShowRight <$> loadKnownHosts khPath
    newMVar (kh, mtime)

loadKnownHosts :: FilePath -> IO (Either String KnownHosts)
loadKnownHosts fpath = AE.eitherDecode' <$> BL.readFile fpath

saveKnownHosts :: KnownHosts -> FilePath -> IO ()
saveKnownHosts kh fpath = writeFileLbs fpath $ encodePretty kh

vcQuery :: FilePath
        -> MVar (KnownHosts, UTCTime)
        -> TrustMode
        -> ValidationCacheQueryCallback
vcQuery khPath khVar t (hostname, _) (Fingerprint b) _ = do
    maybeRefreshKnownHosts khPath khVar
    (kh, _) <- readMVar khVar
    let valid     = found || shouldAdd
        found     = fp `S.member` fps
        fp        = RsaSha512Fingerprint b
        fps       = fromMaybe S.empty $ HM.lookup hostname (khHosts kh)
        shouldAdd = case t of
            Whitelist    -> False
            Ca           -> False
            -- Note: Certificate's hostname is not validated here
            CaOrTofu     -> null fps
            Tofu         -> null fps
            NoValidation -> True
        khAdded   = kh
            { khHosts = HM.insert hostname (S.insert fp fps) (khHosts kh)
            }
    when (not found && shouldAdd) $ do
        warnf "{} ({} trust mode): Adding new fingerprint {} for host {}"
            (khPath, show t, show $ hexWithColons b, hostname)
        saveKnownHosts khAdded khPath
        mtime <- getModificationTime khPath
        void $ swapMVar khVar (khAdded, mtime)
    return $ if valid
        then ValidationCachePass
        else case t of
                 Ca       -> ValidationCacheUnknown
                 CaOrTofu -> ValidationCacheUnknown
                 _        -> ValidationCacheDenied "Failed trust validation"

maybeRefreshKnownHosts :: FilePath -> MVar (KnownHosts, UTCTime) -> IO ()
maybeRefreshKnownHosts khPath khVar = modifyMVar_ khVar $ \orig -> do
    mtime <- getModificationTime khPath
    if mtime > snd orig
        then do
            ekh <- loadKnownHosts khPath
            case ekh of
                Left err -> do
                    warnf "Could not load known hosts file {}: {}" (khPath, err)
                    return orig
                Right kh -> return (kh, mtime)
        else return orig

-- | Called when a fingerprint has passed CA certificate validation after a
-- ValidationCacheUnknown from vcQuery.
vcAdd :: FilePath
      -> MVar (KnownHosts, UTCTime)
      -> TrustMode
      -> ValidationCacheAddCallback
vcAdd khPath khVar t (hostname, _) (Fingerprint b) _
    | t == Ca || t == CaOrTofu = do
          maybeRefreshKnownHosts khPath khVar
          (kh, _) <- readMVar khVar
          let fp      = RsaSha512Fingerprint b
              fps     = fromMaybe S.empty $ HM.lookup hostname (khHosts kh)
              khAdded = kh
                  { khHosts = HM.insert hostname (S.insert fp fps) (khHosts kh)
                  }
          warnf "{} ({} trust mode): Adding new fingerprint '{}' for host '{}'"
              (khPath, show t, show $ hexWithColons b, hostname)
          saveKnownHosts khAdded khPath
    | otherwise                = warnf "vcAdd: Called in non-CA trust mode for host '{}'"
                                 [hostname]
