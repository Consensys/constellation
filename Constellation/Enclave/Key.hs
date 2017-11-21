{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
module Constellation.Enclave.Key where

import Prelude (putStrLn)
import ClassyPrelude hiding (hash, putStrLn)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Data.ByteArray.Encoding (Base(Base64), convertToBase)
import qualified Crypto.Saltine.Class as S
import qualified Crypto.Saltine.Core.Box as Box
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Constellation.Enclave.Types
    (PublicKey(PublicKey, unPublicKey), mkPublicKey)
import Constellation.Util.ByteString (b64TextDecodeBs)
import Constellation.Util.Either
    (fromShowRight, flattenEithers, maybeToExceptT)
import Constellation.Util.File (worriedReadFile)
import Constellation.Util.Lockable
    (Lockable(Unlocked), lock, promptingUnlock, unlock)

newKeyPair :: IO (PublicKey, Box.SecretKey)
newKeyPair = do
    (priv, pub) <- Box.newKeypair
    return (PublicKey pub, priv)

b64EncodePublicKey :: PublicKey -> ByteString
b64EncodePublicKey = convertToBase Base64 . S.encode . unPublicKey

-- | Optionally takes a password to lock the private key.
jsonEncodePrivateKey :: Maybe String -> Box.SecretKey -> IO BL.ByteString
jsonEncodePrivateKey mpwd priv = AE.encode <$> mkLockable
  where
    mkLockable = case mpwd of
        Nothing  -> return $ Unlocked (S.encode priv)
        Just ""  -> return $ Unlocked (S.encode priv)
        Just pwd -> lock pwd (S.encode priv)

loadKeyPair :: (FilePath, FilePath, Maybe String)
            -> IO (Either String (PublicKey, Box.SecretKey))
loadKeyPair (pubPath, privPath, mpwd) = runExceptT $ do
    pub    <- ExceptT $ loadPublicKey pubPath
    locked <- ExceptT $ AE.eitherDecode' . fromStrict <$>
        worriedReadFile privPath
    liftIO $ putStrLn $ "Unlocking " ++ privPath
    privBs <- ExceptT $ case mpwd of
        Just pwd -> return $ unlock pwd locked
        Nothing  -> promptingUnlock locked
    liftIO $ putStrLn $ "Unlocked " ++ privPath
    (pub,) <$> maybeToExceptT "Failed to S.decode privBs" (S.decode privBs)

loadPublicKey :: FilePath -> IO (Either String PublicKey)
loadPublicKey pubPath = runExceptT $ do
    pubBs <- ExceptT $ b64TextDecodeBs . T.strip <$> readFileUtf8 pubPath
    maybeToExceptT "loadKeyPair: Failed to mkPublicKey" (mkPublicKey pubBs)

loadKeyPairs :: [(FilePath, FilePath, Maybe String)]
             -> IO (Either String [(PublicKey, Box.SecretKey)])
loadKeyPairs ks = flattenEithers "; " <$> mapM loadKeyPair ks

mustLoadKeyPairs :: [(FilePath, FilePath, Maybe String)]
                 -> IO [(PublicKey, Box.SecretKey)]
mustLoadKeyPairs ks = fromShowRight <$> loadKeyPairs ks

loadPublicKeys :: [FilePath] -> IO (Either String [PublicKey])
loadPublicKeys ks = flattenEithers "; " <$> mapM loadPublicKey ks

mustLoadPublicKeys :: [FilePath] -> IO [PublicKey]
mustLoadPublicKeys ks = fromShowRight <$> loadPublicKeys ks
