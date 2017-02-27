{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Constellation.Enclave.Key where

import Prelude (putStrLn)
import ClassyPrelude hiding (hash, putStrLn)
import qualified Data.Aeson as AE
import qualified Crypto.Saltine.Class as S
import qualified Crypto.Saltine.Core.Box as Box
import Control.Monad.Trans.Either (EitherT(EitherT), hoistEither, runEitherT)

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
loadKeyPair (pubPath, privPath) = runEitherT $ do
    pubBs <- EitherT $ b64TextDecodeBs <$> readFileUtf8 pubPath
    pub <- hoistMaybe "loadKeyPair: Failed to mkPublicKey" (mkPublicKey pubBs)
    locked <- EitherT $ AE.eitherDecode' . fromStrict <$> readFile privPath
    liftIO $ putStrLn ("Unlocking " ++ privPath)
    privBs <- EitherT $ promptingUnlock locked
    (pub,) <$> hoistMaybe "Failed to S.encode privBs" (S.decode privBs)

  where
    hoistMaybe :: Monad m => e -> Maybe a -> EitherT e m a
    hoistMaybe err may = hoistEither $ maybe (Left err) Right may

loadKeyPairs :: [(FilePath, FilePath)]
             -> IO (Either String [(PublicKey, Box.SecretKey)])
loadKeyPairs kpaths = flattenEithers "; " <$> mapM loadKeyPair kpaths

mustLoadKeyPairs :: [(FilePath, FilePath)] -> IO [(PublicKey, Box.SecretKey)]
mustLoadKeyPairs kpaths = fromShowRight <$> loadKeyPairs kpaths
