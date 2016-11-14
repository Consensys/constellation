{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Constellation.Enclave.Key where

import Prelude (putStrLn)
import ClassyPrelude hiding (hash, putStrLn)
import qualified Data.Aeson as AE
import qualified Crypto.Saltine.Class as S
import qualified Crypto.Saltine.Core.Box as Box

import Constellation.Enclave.Types (PublicKey(PublicKey), mkPublicKey)
import Constellation.Util.ByteString (b64TextDecodeBs)
import Constellation.Util.Either (fromShowRight, flattenEithers)
import Constellation.Util.Lockable (promptingUnlock)

newKeyPair :: IO (PublicKey, Box.SecretKey)
newKeyPair = do
    (priv, pub) <- Box.newKeypair
    return (PublicKey pub, priv)

loadKeyPair :: (FilePath, FilePath)
            -> IO (Either String (PublicKey, Box.SecretKey))
loadKeyPair (pubPath, privPath) = b64TextDecodeBs <$> readFile pubPath >>= \case
    Left err    -> return $ Left err
    Right pubBs -> case mkPublicKey pubBs of
        Nothing  -> return $ Left "loadKeyPair: Failed to mkPublicKey"
        Just pub -> AE.eitherDecode' <$> readFile privPath >>= \case
            Left err     -> return $ Left err
            Right locked -> do
                putStrLn ("Unlocking " ++ privPath)
                promptingUnlock locked >>= \case
                    Left err     -> return $ Left err
                    Right privBs -> case S.decode privBs of
                        Nothing   -> return $ Left "Failed to S.encode privBs"
                        Just priv -> return $ Right (pub, priv)

loadKeyPairs :: [(FilePath, FilePath)]
             -> IO (Either String [(PublicKey, Box.SecretKey)])
loadKeyPairs kpaths = flattenEithers "; " <$> mapM loadKeyPair kpaths

mustLoadKeyPairs :: [(FilePath, FilePath)] -> IO [(PublicKey, Box.SecretKey)]
mustLoadKeyPairs kpaths = fromShowRight <$> loadKeyPairs kpaths
