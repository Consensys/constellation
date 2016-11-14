{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Constellation.Enclave.Keygen.Main where

import ClassyPrelude hiding (getArgs, writeFile)
import System.Console.Haskeline (runInputT, defaultSettings, getPassword)
import System.Environment (getArgs, getProgName)
import Text.Printf (printf)
import qualified Crypto.Saltine.Class as S
import qualified Data.Aeson as AE
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Lazy as BL

import Constellation.Enclave.Key (newKeyPair)
import Constellation.Enclave.Types (PublicKey(unPublicKey))
import Constellation.Util.Lockable (Lockable(Unlocked), lock)
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
    BL.writeFile (name ++ ".pub") $ B64L.encode $ BL.fromStrict $ S.encode $
        unPublicKey pub
    k <- case mpwd of
        Nothing  -> return $ Unlocked (S.encode priv)
        Just ""  -> return $ Unlocked (S.encode priv)
        Just pwd -> lock pwd (S.encode priv)
    BL.writeFile (name ++ ".key") $ AE.encode k

usage :: IO ()
usage = getProgName >>= \progName ->
    putStrLn $ tformat "Usage: {} <keypair name> <keypair name>..." progName
