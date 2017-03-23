{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Constellation.Enclave.Key where

import Prelude (putStrLn)
import ClassyPrelude hiding (hash, putStrLn)
import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT)
import qualified Data.Aeson as AE
import qualified Crypto.Saltine.Class as S
import qualified Crypto.Saltine.Core.Box as Box

import Constellation.Enclave.Types (PublicKey(PublicKey), mkPublicKey)
import Constellation.Util.ByteString (b64TextDecodeBs)
import Constellation.Util.Either (fromShowRight, flattenEithers, maybeToEitherT)
import Constellation.Util.Lockable (unlock, promptingUnlock)

newKeyPair :: IO (PublicKey, Box.SecretKey)
newKeyPair = do
    (priv, pub) <- Box.newKeypair
    return (PublicKey pub, priv)

loadKeyPair :: (FilePath, FilePath, Maybe String)
            -> IO (Either String (PublicKey, Box.SecretKey))
loadKeyPair (pubPath, privPath, mpwd) = runEitherT $ do
    pubBs  <- EitherT $ b64TextDecodeBs <$> readFileUtf8 pubPath
    pub    <- maybeToEitherT "loadKeyPair: Failed to mkPublicKey"
              (mkPublicKey pubBs)
    locked <- EitherT $ AE.eitherDecode' . fromStrict <$> readFile privPath
    liftIO $ putStrLn $ "Unlocking " ++ privPath
    privBs <- EitherT $ case mpwd of
        Just pwd -> return $ unlock pwd locked
        Nothing  -> promptingUnlock locked
    liftIO $ putStrLn $ "Unlocked" ++ privPath
    (pub,) <$> maybeToEitherT "Failed to S.encode privBs" (S.decode privBs)

loadKeyPairs :: [(FilePath, FilePath, Maybe String)]
             -> IO (Either String [(PublicKey, Box.SecretKey)])
loadKeyPairs ks = flattenEithers "; " <$> mapM loadKeyPair ks

mustLoadKeyPairs :: [(FilePath, FilePath, Maybe String)]
                 -> IO [(PublicKey, Box.SecretKey)]
mustLoadKeyPairs ks = fromShowRight <$> loadKeyPairs ks
