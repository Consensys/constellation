{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Main where

import ClassyPrelude hiding (getArgs, log)
import Control.Concurrent (forkIO)
import Control.Logging
    ( LogLevel(LevelDebug, LevelInfo, LevelWarn, LevelError)
    , setLogLevel, withStderrLogging, log', errorL'
    )
import Data.Text.Format (Shown(Shown))
import GHC.Conc (getNumProcessors)
import Network.Socket
    ( Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix)
    , socket, bind, listen, maxListenQueue, close
    )
import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp

import Constellation.Enclave
    (newEnclave', enclaveEncryptPayload, enclaveDecryptPayload)
import Constellation.Enclave.Key (mustLoadKeyPairs, mustLoadPublicKeys)
import Constellation.Enclave.Keygen.Main (generateKeyPair)
import Constellation.Node (newNode, runNode)
import Constellation.Node.Storage.BerkeleyDb (berkeleyDbStorage)
import Constellation.Node.Storage.Directory (directoryStorage)
import Constellation.Node.Storage.LevelDb (levelDbStorage)
import Constellation.Node.Storage.Memory (memoryStorage)
import Constellation.Node.Storage.Sqlite (sqliteStorage)
import Constellation.Node.Types
    ( Node(nodeStorage)
    , Crypt(Crypt, encryptPayload, decryptPayload)
    , Storage(closeStorage)
    )
import Constellation.Node.Config (Config(..), extractConfig)
import Constellation.Util.AtExit (registerAtExit, withAtExit)
import Constellation.Util.Logging (debugf', logf', warnf)
import qualified Constellation.Node.Api as NodeApi

version :: Text
version = "0.1.0"

defaultMain :: IO ()
defaultMain = do
    args     <- getArgs
    (cfg, _) <- extractConfig args
    if cfgJustShowVersion cfg
        then putStrLn ("Constellation Node " ++ version)
        else case cfgJustGenerateKeys cfg of
                 [] -> withStderrLogging $ run cfg
                 ks -> mapM_ generateKeyPair ks

run :: Config -> IO ()
run cfg@Config{..} = do
    let logLevel = case cfgVerbosity of
            0 -> LevelError
            1 -> LevelWarn
            2 -> LevelInfo
            3 -> LevelDebug
            _ -> LevelDebug
    logf' "Log level is {}" [show logLevel]
    setLogLevel logLevel
    debugf' "Configuration: {}" [show cfg]
    ncpus <- getNumProcessors
    logf' "Utilizing {} core(s)" [ncpus]
    setNumCapabilities ncpus
    pwds <- case cfgPasswords of
        Just passPath -> (map (Just . T.unpack) . lines) <$>
            readFileUtf8 passPath
        Nothing       -> return $ replicate (length cfgPublicKeys) Nothing
    when (length cfgPublicKeys /= length cfgPrivateKeys) $
        errorL' "The same amount of public keys and private keys must be specified"
    when (length cfgPublicKeys /= length pwds) $
        errorL' "The same amount of passwords must be included in the passwords file as the number of private keys. (If a private key has no password, include a blank line.)"
    when (cfgPort == 0) $
        errorL' "A listening port must be specified with 'port' in the configuration file or --port at runtime"
    let kps = zip3 cfgPublicKeys cfgPrivateKeys pwds
    logf' "Constructing Enclave using keypairs {}"
        [show $ zip cfgPublicKeys cfgPrivateKeys]
    ks <- mustLoadKeyPairs kps
    e  <- newEnclave' ks
    let crypt = Crypt
            { encryptPayload = enclaveEncryptPayload e
            , decryptPayload = enclaveDecryptPayload e
            }
    ast <- mustLoadPublicKeys cfgAlwaysSendTo
    logf' "Initializing storage {}" [cfgStorage]
    let experimentalStorageCaveat s = warnf "The {} storage engine is experimental. It may be removed or changed at any time. Please see the discussion at https://github.com/jpmorganchase/constellation/issues/37" [s :: Text]
    storage <- case break (== ':') cfgStorage of
        ("bdb",     ':':path) -> berkeleyDbStorage path
        ("dir",     ':':path) -> directoryStorage path
        ("leveldb", ':':path) -> experimentalStorageCaveat "LevelDB"
            >> levelDbStorage path
        ("memory",  _       ) -> memoryStorage
        ("sqlite",  ':':path) -> experimentalStorageCaveat "SQLite"
            >> sqliteStorage path
        _                     -> berkeleyDbStorage cfgStorage  -- Default
    nvar    <- newTVarIO =<<
        newNode crypt storage cfgUrl (map fst ks) ast cfgOtherNodes
    _ <- forkIO $ do
        let mwl = if null cfgIpWhitelist
                then Nothing
                else Just $ NodeApi.whitelist cfgIpWhitelist
        logf' "Public API listening on 0.0.0.0 port {} with whitelist: {}"
            ( cfgPort
            , Shown $ if isNothing mwl then ["Disabled"] else cfgIpWhitelist
            )
        Warp.run cfgPort $ NodeApi.app mwl NodeApi.Public nvar
    _ <- case cfgSocket of
        Just sockPath -> void $ forkIO $ do
            logf' "Internal API listening on {}" [sockPath]
            resetSocket sockPath
            sock <- socket AF_UNIX Stream 0
            bind sock $ SockAddrUnix sockPath
            listen sock maxListenQueue
            Warp.runSettingsSocket Warp.defaultSettings sock $
                NodeApi.app Nothing NodeApi.Private nvar
            close sock
        Nothing       -> return ()
    registerAtExit $ do
        log' "Shutting down... (Interrupting this will cause the next startup to take longer)"
        case cfgSocket of
            Just sockPath -> resetSocket sockPath
            Nothing       -> return ()
        readTVarIO nvar >>= closeStorage . nodeStorage
    log' "Node started"
    withAtExit $ runNode nvar

resetSocket :: FilePath -> IO ()
resetSocket sockPath = doesFileExist sockPath >>= \exists ->
    when exists $ removeFile sockPath
