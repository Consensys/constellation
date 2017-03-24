{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
module Constellation.Node.Main where

import ClassyPrelude hiding (log)
import Control.Concurrent (forkIO)
import Control.Logging
    (LogLevel(LevelWarn), setLogLevel, withStderrLogging, log')
import Data.Text.Format (Shown(Shown))
import GHC.Conc (getNumProcessors)
import Network.Socket
    ( Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix)
    , socket, bind, listen, maxListenQueue, close
    )
import System.Directory (doesFileExist, removeFile)
import System.Environment (getProgName)
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import qualified Network.Wai.Handler.Warp as Warp

import Constellation.Enclave
    (newEnclave', enclaveEncryptPayload, enclaveDecryptPayload)
import Constellation.Enclave.Key (mustLoadKeyPairs)
import Constellation.Node (newNode, runNode)
import Constellation.Node.Storage.BerkeleyDb (berkeleyDbStorage)
-- import Constellation.Node.Storage.Memory (memoryStorage)
import Constellation.Node.Types
    ( Node(nodeStorage)
    , Crypt(Crypt, encryptPayload, decryptPayload)
    , Storage(closeStorage)
    )
import Constellation.Node.Config (Config(..), loadConfigFile)
import Constellation.Util.AtExit (registerAtExit, withAtExit)
import Constellation.Util.Logging (logf', warnf')
import qualified Constellation.Node.Api as NodeApi

defaultMain :: IO ()
defaultMain = do
    args <- getArgs
    case args of
        [cfgPath] -> withStderrLogging $ do
            logf' "Constellation initializing using config file {}" [cfgPath]
            loadConfigFile (T.unpack cfgPath) >>= \ecfg -> case ecfg of
                Left err  -> warnf' "Error parsing configuration file: {}" [err]
                Right cfg -> run cfg
        _         -> usage

run :: Config -> IO ()
run Config{..} = do
    let logLevel = LevelWarn
    logf' "Log level is {}" [show logLevel]
    setLogLevel logLevel
    ncpus <- getNumProcessors
    logf' "Utilizing {} core(s)" [ncpus]
    setNumCapabilities ncpus
    let kps = [(cfgPublicKeyPath, cfgPrivateKeyPath)]
    logf' "Constructing Enclave using keypair ({}, {})"
        [cfgPublicKeyPath, cfgPrivateKeyPath]
    ks@[(pub, _)] <- mustLoadKeyPairs kps
    e <- newEnclave' ks
    let crypt = Crypt
            { encryptPayload = enclaveEncryptPayload e
            , decryptPayload = enclaveDecryptPayload e
            }
    logf' "Initializing storage {}" [cfgStoragePath]
    storage <- berkeleyDbStorage cfgStoragePath
    -- storage <- memoryStorage
    nvar    <- newTVarIO =<<
        newNode crypt storage cfgUrl [pub]
        cfgOtherNodeUrls
    _ <- forkIO $ do
        let mwl = if null cfgIpWhitelist
                then Nothing
                else Just $ NodeApi.whitelist cfgIpWhitelist
        logf' "External API listening on 0.0.0.0 port {} with whitelist: {}"
            ( cfgPort
            , Shown $ if isNothing mwl then ["Disabled"] else cfgIpWhitelist
            )
        Warp.run cfgPort $ NodeApi.app mwl False nvar
    _ <- forkIO $ do
        let sockPath = T.unpack cfgSocketPath
        logf' "Internal API listening on {}" [sockPath]
        resetSocket sockPath
        sock <- socket AF_UNIX Stream 0
        bind sock $ SockAddrUnix sockPath
        listen sock maxListenQueue
        Warp.runSettingsSocket Warp.defaultSettings sock $
            NodeApi.app Nothing True nvar
        close sock
    registerAtExit $ do
        log' "Shutting down... (Interrupting this will cause the next startup to take longer)"
        resetSocket $ T.unpack cfgSocketPath
        readTVarIO nvar >>= closeStorage . nodeStorage
    log' "Node started"
    withAtExit $ runNode nvar

resetSocket :: FilePath -> IO ()
resetSocket sockPath = doesFileExist sockPath >>= \exists ->
    when exists $ removeFile sockPath

usage :: IO ()
usage = do
    name <- getProgName
    TF.print "Usage: {} config.toml\n" [name]
