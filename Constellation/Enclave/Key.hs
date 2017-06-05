{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
module Constellation.Enclave.Key where

import Prelude (putStrLn)
import ClassyPrelude hiding (hash, putStrLn)
import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT)
import qualified Crypto.Saltine.Class as S
import qualified Crypto.Saltine.Core.Box as Box
import qualified Data.Aeson as AE
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Constellation.Enclave.Types
    (PublicKey(PublicKey, unPublicKey), mkPublicKey)
import Constellation.Util.ByteString (b64TextDecodeBs)
import Constellation.Util.Either (fromShowRight, flattenEithers, maybeToEitherT)
import Constellation.Util.Lockable
    (Lockable(Unlocked), lock, promptingUnlock, unlock)

newKeyPair :: IO (PublicKey, Box.SecretKey)
newKeyPair = do
    (priv, pub) <- Box.newKeypair
    return (PublicKey pub, priv)

b64EncodePublicKey :: PublicKey -> BL.ByteString
b64EncodePublicKey = B64L.encode . BL.fromStrict . S.encode . unPublicKey

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
loadKeyPair (pubPath, privPath, mpwd) = runEitherT $ do
    pub    <- EitherT $ loadPublicKey pubPath
    locked <- EitherT $ AE.eitherDecode' . fromStrict <$> readFile privPath
    liftIO $ putStrLn $ "Unlocking " ++ privPath
    privBs <- EitherT $ case mpwd of
        Just pwd -> return $ unlock pwd locked
        Nothing  -> promptingUnlock locked
    liftIO $ putStrLn $ "Unlocked " ++ privPath
    (pub,) <$> maybeToEitherT "Failed to S.decode privBs" (S.decode privBs)

loadPublicKey :: FilePath -> IO (Either String PublicKey)
loadPublicKey pubPath = runEitherT $ do
    pubBs <- EitherT $ b64TextDecodeBs . T.strip <$> readFileUtf8 pubPath
    maybeToEitherT "loadKeyPair: Failed to mkPublicKey" (mkPublicKey pubBs)

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
