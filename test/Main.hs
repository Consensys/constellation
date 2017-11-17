{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Main
    (
      main
    ) where

import ClassyPrelude
import Control.Logging (LogLevel(LevelError), setLogLevel, withStderrLogging)
import GHC.Conc (getNumProcessors)
import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Constellation.Enclave.Test as Enclave
import qualified Constellation.Enclave.Main.Test as EnclaveMain
import qualified Constellation.Enclave.Payload.Test as EnclavePayload
import qualified Constellation.Enclave.Types.Test as EnclaveTypes
import qualified Constellation.Node.Test as Node
import qualified Constellation.Node.Api.Test as NodeApi
import qualified Constellation.Node.Config.Test as NodeConfig
import qualified Constellation.Node.Main.Test as NodeMain
import qualified Constellation.Node.Storage.BerkeleyDb.Test as NodeStorageBerkeley
import qualified Constellation.Node.Storage.Directory.Test as NodeStorageDirectory
import qualified Constellation.Node.Storage.LevelDb.Test as NodeStorageLevelDb
import qualified Constellation.Node.Storage.Memory.Test as NodeStorageMemory
import qualified Constellation.Node.Storage.Sqlite.Test as NodeStorageSqlite
import qualified Constellation.Node.Trust.Test as NodeTrust
import qualified Constellation.Node.Types.Test as NodeTypes
import qualified Constellation.Util.AtExit.Test as UtilAtExit
import qualified Constellation.Util.ByteString.Test as UtilByteString
import qualified Constellation.Util.Either.Test as UtilEither
import qualified Constellation.Util.Exception.Test as UtilException
import qualified Constellation.Util.File.Test as UtilFile
import qualified Constellation.Util.Http.Test as UtilHttp
import qualified Constellation.Util.Json.Test as UtilJson
import qualified Constellation.Util.Lockable.Test as UtilLockable
import qualified Constellation.Util.Logging.Test as UtilLogging
import qualified Constellation.Util.Network.Test as UtilNetwork
import qualified Constellation.Util.String.Test as UtilString
import qualified Constellation.Util.Text.Test as UtilText
import qualified Constellation.Util.Tls.Test as UtilTls
import qualified Constellation.Util.Wai.Test as UtilWai

tests :: TestTree
tests = testGroup ""
    [ Enclave.tests
    , EnclaveMain.tests
    , EnclavePayload.tests
    , EnclaveTypes.tests
    , Node.tests
    , NodeApi.tests
    , NodeConfig.tests
    , NodeMain.tests
    , NodeStorageBerkeley.tests
    , NodeStorageDirectory.tests
    , NodeStorageLevelDb.tests
    , NodeStorageMemory.tests
    , NodeStorageSqlite.tests
    , NodeTrust.tests
    , NodeTypes.tests
    , UtilAtExit.tests
    , UtilByteString.tests
    , UtilEither.tests
    , UtilException.tests
    , UtilFile.tests
    , UtilHttp.tests
    , UtilJson.tests
    , UtilLockable.tests
    , UtilLogging.tests
    , UtilNetwork.tests
    , UtilString.tests
    , UtilText.tests
    , UtilTls.tests
    , UtilWai.tests
    ]

main :: IO ()
main = do
    setNumCapabilities =<< getNumProcessors
    setLogLevel LevelError
    withStderrLogging $ defaultMain tests
