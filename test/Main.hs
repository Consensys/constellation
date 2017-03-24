{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Main
    (
      main
    ) where

import ClassyPrelude
import Control.Logging (LogLevel(LevelWarn), setLogLevel, withStderrLogging)
import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Constellation.Enclave.Test as Enclave
import qualified Constellation.Enclave.Main.Test as EnclaveMain
import qualified Constellation.Enclave.Payload.Test as EnclavePayload
import qualified Constellation.Enclave.Types.Test as EnclaveTypes
import qualified Constellation.Node.Test as Node
import qualified Constellation.Node.Api.Test as NodeApi
import qualified Constellation.Node.Config.Test as NodeConfig
import qualified Constellation.Node.Main.Test as NodeMain
-- import qualified Constellation.Node.Storage.LevelDb.Test as NodeStorageLevelDb
import qualified Constellation.Node.Storage.Memory.Test as NodeStorageMemory
import qualified Constellation.Node.Storage.Sqlite.Test as NodeStorageSqlite
import qualified Constellation.Node.Types.Test as NodeTypes
import qualified Constellation.Util.AtExit.Test as UtilAtExit
import qualified Constellation.Util.ByteString.Test as UtilByteString
import qualified Constellation.Util.Either.Test as UtilEither
import qualified Constellation.Util.Exception.Test as UtilException
import qualified Constellation.Util.Lockable.Test as UtilLockable
import qualified Constellation.Util.Memory.Test as UtilMemory
import qualified Constellation.Util.Network.Test as UtilNetwork
import qualified Constellation.Util.String.Test as UtilString
import qualified Constellation.Util.Text.Test as UtilText
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
    -- , NodeStorageLevelDb.tests
    , NodeStorageMemory.tests
    , NodeStorageSqlite.tests
    , NodeTypes.tests
    , UtilAtExit.tests
    , UtilByteString.tests
    , UtilEither.tests
    , UtilException.tests
    , UtilLockable.tests
    , UtilMemory.tests
    , UtilNetwork.tests
    , UtilString.tests
    , UtilText.tests
    , UtilWai.tests
    ]

main :: IO ()
main = do
    setNumCapabilities =<< getNumCapabilities
    setLogLevel LevelWarn
    withStderrLogging $ defaultMain tests
