{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Enclave.Keygen.Main where

import ClassyPrelude hiding (getArgs, writeFile)
import System.Console.Haskeline (runInputT, defaultSettings, getPassword)
import System.Environment (getArgs, getProgName)
import Text.Printf (printf)

import Constellation.Enclave.Key
    (newKeyPair, b64EncodePublicKey, jsonEncodePrivateKey)
import Constellation.Util.File (withOwnerReadWrite, writeFileBs, writeFileLbs)
import Constellation.Util.Text (tformat)

defaultMain :: IO ()
defaultMain = getArgs >>= \case
    [] -> usage
    xs -> mapM_ generateKeyPair xs

generateKeyPair :: String -> IO ()
generateKeyPair name = do
    mpwd <- runInputT defaultSettings $
        getPassword (Just '*') (printf "Lock key pair %s with password [none]: " name)
    (pub, priv) <- newKeyPair
    writeFileBs (name ++ ".pub") $ b64EncodePublicKey pub
    json <- jsonEncodePrivateKey mpwd priv
    withOwnerReadWrite $ writeFileLbs (name ++ ".key") json

usage :: IO ()
usage = getProgName >>= \progName ->
    putStrLn $ tformat "Usage: {} <keypair name> <keypair name>..." progName
