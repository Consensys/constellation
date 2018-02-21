{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Main where

import ClassyPrelude hiding (getArgs, log)
import Control.Concurrent (forkIO)
import Control.Logging
    ( LogLevel(LevelDebug, LevelInfo, LevelWarn, LevelError)
    , setLogLevel, withStderrLogging, log, errorL
    )
import Data.Text.Format (Shown(Shown))
import Data.X509
    (CertificateChain(..), HashALG(..), certValidity, getCertificate)
import Data.X509.Validation (Fingerprint(..), getFingerprint)
import GHC.Conc (getNumProcessors)
import Network.HTTP.Conduit
    ( Manager, ManagerSettings(..)
    , tlsManagerSettings, managerConnCount, newManager, parseRequest, host
    )
import Network.Socket
    ( Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix)
    , socket, bind, listen, maxListenQueue, close
    )
import Network.TLS (credentialLoadX509Chain)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Settings, runSettings)
import Network.Wai.Handler.WarpTLS (TLSSettings(..), runTLS)
import System.Directory
    (setCurrentDirectory, createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (getArgs)
import System.Posix.Files (setFileMode)
import Text.Pretty.Simple (pShowNoColor)
import Time.System (dateCurrent)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Wai.Handler.Warp as Warp

import Constellation.Enclave
    (newEnclave', enclaveEncryptPayload, enclaveDecryptPayload)
import Constellation.Enclave.Key
    (newKeyPair, mustLoadKeyPairs, mustLoadPublicKeys)
import Constellation.Enclave.Keygen.Main (generateKeyPair)
import Constellation.Enclave.Types (PublicKey)
import Constellation.Node (newNode, runNode)
import Constellation.Node.Api (ApiType(..), apiApp, whitelist)
import Constellation.Node.Config (Config(..), extractConfig)
import Constellation.Node.Storage.BerkeleyDb (berkeleyDbStorage)
import Constellation.Node.Storage.Directory (directoryStorage)
import Constellation.Node.Storage.LevelDb (levelDbStorage)
import Constellation.Node.Storage.Memory (memoryStorage)
import Constellation.Node.Storage.Sqlite (sqliteStorage)
import Constellation.Node.Trust
    ( TrustMode(..), tlsSettings, validationCache, managerSettings
    , stringToTrustMode, certHostname
    )
import Constellation.Node.Types
    ( Node(nodeStorage)
    , Crypt(Crypt, encryptPayload, decryptPayload)
    , Storage(closeStorage)
    )
import Constellation.Util.AtExit (registerAtExit, withAtExit)
import Constellation.Util.ByteString (hexWithColons)
import Constellation.Util.Either (fromShowRight)
import Constellation.Util.File (worryAboutPermissions)
import Constellation.Util.Logging (debugf', logf', warnf', errorf')
import Constellation.Util.Tls (newSelfSignedCertificate)

version :: Text
version = "0.3.2"

defaultMain :: IO ()
defaultMain = do
    args     <- getArgs
    (cfg, _) <- extractConfig args
    if cfgJustShowVersion cfg
        then putStrLn ("Constellation Node " ++ version)
        else do
            _ <- case cfgWorkDir cfg of
                Nothing -> return ()
                Just wd -> do
                    createDirectoryIfMissing True wd
                    setCurrentDirectory wd
            case cfgJustGenerateKeys cfg of
                 [] -> withStderrLogging $ run cfg
                 ks -> mapM_ generateKeyPair ks

run :: Config -> IO ()
run cfg@Config{..} = do
    setupLogging cfgVerbosity
    debugf' "Configuration: {}" [pShowNoColor cfg]
    sanityCheckConfig cfg
    setupParallelism
    logf' "Constructing Enclave using keypairs {}"
        [show $ zip cfgPublicKeys cfgPrivateKeys]
    (crypt, pubs) <- setupCrypt cfgPublicKeys cfgPrivateKeys cfgPasswords
    ast           <- mustLoadPublicKeys cfgAlwaysSendTo
    (selfPub, _)  <- newKeyPair
    logf' "Throwaway public key for self-sending: {}" [show selfPub]
    storage                  <- setupStorage cfgStorage
    (warpFunc, m, setSecure) <- setupTls cfg
    nvar <- newTVarIO =<<
        newNode crypt storage cfgUrl pubs ast selfPub cfgOtherNodes m
        setSecure
    _    <- forkIO $ do
        let mwl = if null cfgIpWhitelist
                then Nothing
                else Just $ whitelist cfgIpWhitelist
        logf' "Public API listening on 0.0.0.0 port {} with whitelist: {}"
            ( cfgPort
            , Shown $ if isNothing mwl then ["Disabled"] else cfgIpWhitelist
            )
        warpFunc (Warp.setPort cfgPort Warp.defaultSettings) $
            apiApp mwl Public nvar
    _       <- case cfgSocket of
        Just sockPath -> void $ forkIO $ runPrivateApi sockPath nvar
        Nothing       -> return ()
    registerAtExit $ do
        log "Shutting down... (Interrupting this will cause the next startup to take longer)"
        case cfgSocket of
            Just sockPath -> resetSocket sockPath
            Nothing       -> return ()
        readTVarIO nvar >>= closeStorage . nodeStorage
    log "Node started"
    withAtExit $ runNode nvar

setupLogging :: Int -> IO ()
setupLogging v = logf' "Log level is {}" [show logLevel] >> setLogLevel logLevel
  where
    logLevel = intToLogLevel v

intToLogLevel :: Int -> LogLevel
intToLogLevel 0 = LevelError
intToLogLevel 1 = LevelWarn
intToLogLevel 2 = LevelInfo
intToLogLevel _ = LevelDebug

sanityCheckConfig :: Config -> IO ()
sanityCheckConfig Config{..} = do
    when (length cfgPublicKeys /= length cfgPrivateKeys) $
        errorL "The same amount of public keys and private keys must be specified"
    when (cfgPort == 0) $
        errorL "A listening port must be specified with 'port' in the configuration file or --port at runtime"

setupParallelism :: IO ()
setupParallelism = do
    ncpus <- getNumProcessors
    logf' "Utilizing {} core(s)" [ncpus]
    setNumCapabilities ncpus

setupCrypt :: [FilePath]
           -> [FilePath]
           -> Maybe FilePath
           -> IO (Crypt, [PublicKey])
setupCrypt pubs privs mpwds = do
    pwds <- case mpwds of
        Just passPath -> (map (Just . T.unpack) . lines) <$>
            readFileUtf8 passPath
        Nothing       -> return $ replicate (length pubs) Nothing
    when (length pubs /= length pwds) $
        errorL "The same amount of passwords must be included in the passwords file as the number of private keys. (If a private key has no password, include a blank line.)"
    let kps = zip3 pubs privs pwds
    ks <- mustLoadKeyPairs kps
    e  <- newEnclave' ks
    let crypt = Crypt
            { encryptPayload = enclaveEncryptPayload e
            , decryptPayload = enclaveDecryptPayload e
            }
    return (crypt, map fst ks)

setupStorage :: String -> IO Storage
setupStorage storage = case break (== ':') storage of
    ("bdb",     ':':path) -> berkeleyDbStorage path
    ("dir",     ':':path) -> directoryStorage path
    ("leveldb", ':':path) -> experimentalStorageCaveat "LevelDB"
        >> levelDbStorage path
    ("memory",  _       ) -> memoryStorage
    ("sqlite",  ':':path) -> experimentalStorageCaveat "SQLite"
        >> sqliteStorage path
    _                     -> berkeleyDbStorage storage  -- Default
  where
    experimentalStorageCaveat s = warnf' "The {} storage engine is experimental. It may be removed or changed at any time. Please see the discussion at https://github.com/jpmorganchase/constellation/issues/37" [s :: Text]

setupTls :: Config -> IO (Settings -> Application -> IO (), Manager, Bool)
setupTls Config{..} = case cfgTls of
    -- TLS is disabled, but unauthenticated outbound TLS connections can
    -- still happen.
    "off"    -> newManager (additionalManagerSettings tlsManagerSettings)
        >>= \m -> return (runSettings, m, False)
    "strict" -> do
        hostname <- hostnameFromUrl cfgUrl
        m        <- setupClientTls cfgTlsClientCert cfgTlsClientChain
            cfgTlsClientKey cfgTlsKnownServers ctrust hostname
        settings <- setupServerTls cfgTlsServerCert cfgTlsServerChain
            cfgTlsServerKey cfgTlsKnownClients strust hostname
        sanityCheckCredential cfgTlsServerCert cfgTlsServerChain
            cfgTlsServerKey hostname
        sanityCheckCredential cfgTlsClientCert cfgTlsClientChain
            cfgTlsClientKey hostname
        return (runTLS settings, m, True)
    _        -> error "setupTls: Invalid TLS mode"
  where
    strust   = fromMaybe (error "Invalid server trust mode") $
               stringToTrustMode cfgTlsServerTrust
    ctrust   = fromMaybe (error "Invalid client trust mode") $
               stringToTrustMode cfgTlsClientTrust

hostnameFromUrl :: MonadThrow m => Text -> m Text
hostnameFromUrl url = do
    req <- parseRequest (T.unpack url)
    return $ TE.decodeUtf8 $ host req

sanityCheckCredential :: FilePath -> [FilePath] -> FilePath -> Text -> IO ()
sanityCheckCredential certPath chain keyPath hostname = do
    (CertificateChain (signed:_), _) <- fromShowRight <$>
        credentialLoadX509Chain certPath chain keyPath
    now <- dateCurrent
    let cert                 = getCertificate signed
        (validFrom, validTo) = certValidity cert
    _ <- case certHostname cert of
        Nothing       ->
            warnf' "{} doesn't have a hostname. This may cause connection problems."
            [certPath]
        Just certHost -> when (certHost /= T.unpack hostname) $
            warnf' "{}'s hostname ({}) is different from the one given in the configuration ({}). This may cause connection problems."
            (certPath, certHost, hostname)
    when (validFrom > now) $
        warnf' "{} is not valid yet (valid from {} to {}). This may cause connection problems."
        (certPath, show validFrom, show validTo)
    -- TODO: Warn when expiration is close
    when (validTo < now) $
        warnf' "{} has expired (valid from {} to {}). This may cause connection problems."
        (certPath, show validFrom, show validTo)
    let (Fingerprint fp) = getFingerprint signed HashSHA512
    logf' "{}'s fingerprint is {}" (certPath, show $ hexWithColons fp)

runPrivateApi :: FilePath -> TVar Node -> IO ()
runPrivateApi sockPath nvar = do
    logf' "Private API listening on {}" [sockPath]
    resetSocket sockPath
    sock <- socket AF_UNIX Stream 0
    bind sock $ SockAddrUnix sockPath
    listen sock maxListenQueue
    setFileMode sockPath 33152
    Warp.runSettingsSocket Warp.defaultSettings sock $
        apiApp Nothing Private nvar
    close sock

setupServerTls :: FilePath
               -> [FilePath]
               -> FilePath
               -> FilePath
               -> TrustMode
               -> Text
               -> IO TLSSettings
setupServerTls certPath chainPaths keyPath knownClientsPath t hostname = do
    maybeCreateCertAndKey certPath keyPath hostname
    tlsSettings certPath chainPaths keyPath knownClientsPath t

maybeCreateCertAndKey :: FilePath -> FilePath -> Text -> IO ()
maybeCreateCertAndKey certPath keyPath hostname = do
    certExists <- doesFileExist certPath
    keyExists  <- doesFileExist keyPath
    _          <- when (certExists && not keyExists) $
        errorf' "The TLS certificate {} was found, but the key was not" [certPath]
    _          <- when (not certExists && keyExists) $
        errorf' "The TLS key {} was found, but the certificate was not" [keyPath]
    _          <- when (not certExists && not keyExists) $ do
        warnf' "No TLS certificate or key found; generating {}/{}"
            [certPath, keyPath]
        newSelfSignedCertificate certPath keyPath hostname
    _          <- when (certExists && keyExists) $
        return ()
    worryAboutPermissions keyPath

setupClientTls :: FilePath
               -> [FilePath]
               -> FilePath
               -> FilePath
               -> TrustMode
               -> Text
               -> IO Manager
setupClientTls certPath chainPaths keyPath knownServersPath t hostname = do
    vc <- validationCache knownServersPath t
    maybeCreateCertAndKey certPath keyPath hostname
    cred     <- fromShowRight <$>
                credentialLoadX509Chain certPath chainPaths keyPath
    settings <- managerSettings vc cred t
    -- I'm Mr. Manager
    newManager $ additionalManagerSettings settings

additionalManagerSettings :: ManagerSettings -> ManagerSettings
additionalManagerSettings settings = settings
    { managerConnCount = 100
    }

resetSocket :: FilePath -> IO ()
resetSocket sockPath = doesFileExist sockPath >>= \exists ->
    when exists $ removeFile sockPath
