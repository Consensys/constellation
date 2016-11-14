{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Constellation.Util.Lockable where

import ClassyPrelude hiding (hash)
import Crypto.Argon2
    ( HashOptions( HashOptions, hashIterations, hashMemory, hashParallelism
                 , hashVariant
                 )
    , Argon2Variant(Argon2d, Argon2i)
    )
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), (.:), (.=), object)
import System.Console.Haskeline (runInputT, defaultSettings, getPassword)
import System.Entropy (getEntropy)
import qualified Data.Aeson as AE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Crypto.Saltine.Class as S
import qualified Crypto.Saltine.Core.SecretBox as SBox

import Constellation.Util.Argon2 (hash)
import Constellation.Util.ByteString (b64TextEncodeBs, b64TextDecodeBs)

defaultArgonOptions :: ArgonOptions
defaultArgonOptions = ArgonOptions HashOptions
    { hashIterations  = 10
    , hashMemory      = (2 :: Word32) ^ (20 :: Word32)  -- 1 GiB
    , hashParallelism = 4
    , hashVariant     = Argon2i
    }

data Lockable = Unlocked ByteString
              | Argon2SBox ArgonSalt ArgonOptions SBoxNonce ByteString

instance ToJSON Lockable where
    toJSON (Unlocked b)                      = object
        [ "type" .= ("unlocked" :: Text)
        , "data" .= object ["bytes" .= b64TextEncodeBs b]
        ]
    toJSON (Argon2SBox asalt aopts snonce b) = object
        [ "type" .= ("argon2sbox" :: Text)
        , "data" .= object
              [ "asalt"  .= asalt
              , "aopts"  .= aopts
              , "snonce" .= snonce
              , "sbox"   .= b64TextEncodeBs b
              ]
        ]

instance FromJSON Lockable where
    parseJSON (AE.Object v) = do
        t <- v .: "type"
        d <- v .: "data"
        case t :: Text of
            "unlocked"   -> d .: "bytes" >>= \s -> case b64TextDecodeBs s of
                Left err -> fail err
                Right b  -> return $ Unlocked b
            "argon2sbox" -> do
                esbox <- b64TextDecodeBs <$> d .: "sbox"
                case esbox of
                    Left err   -> fail err
                    Right sbox -> Argon2SBox
                        <$> d .: "asalt"
                        <*> d .: "aopts"
                        <*> d .: "snonce"
                        <*> pure sbox
            _            -> fail "Unknown Lockable type"
    parseJSON _             = fail "Lockable must be an Aeson Object"

newtype ArgonSalt = ArgonSalt { unArgonSalt :: ByteString }

instance ToJSON ArgonSalt where
    toJSON (ArgonSalt asalt) = toJSON $ b64TextEncodeBs asalt

instance FromJSON ArgonSalt where
    parseJSON (AE.String s) = case b64TextDecodeBs s of
        Left err    -> fail err
        Right asalt -> return $ ArgonSalt asalt
    parseJSON _             = fail "ArgonSalt must be an Aeson String"

newtype ArgonOptions = ArgonOptions { unArgonOptions :: HashOptions }

instance ToJSON ArgonOptions where
    toJSON (ArgonOptions HashOptions{..}) = object
        [ "iterations"  .= hashIterations
        , "memory"      .= hashMemory
        , "parallelism" .= hashParallelism
        , "variant"     .= case hashVariant of
              Argon2i -> "i" :: Text
              Argon2d -> "d" :: Text
        ]

instance FromJSON ArgonOptions where
    parseJSON (AE.Object v) = do
        iter   <- v .: "iterations"
        mem    <- v .: "memory"
        par    <- v .: "parallelism"
        varStr <- v .: "variant"
        var    <- case varStr :: Text of
            "i" -> return Argon2i
            "d" -> return Argon2d
            _   -> fail "Unrecognized Argon2 variant"
        return $ ArgonOptions HashOptions
            { hashIterations  = iter
            , hashMemory      = mem
            , hashParallelism = par
            , hashVariant     = var
            }
    parseJSON _             = fail "ArgonOptions must be an Aeson Object"

newtype SBoxNonce = SBoxNonce SBox.Nonce

instance ToJSON SBoxNonce where
    toJSON (SBoxNonce snonce) = toJSON $ b64TextEncodeBs $ S.encode snonce

instance FromJSON SBoxNonce where
    parseJSON (AE.String s) = case b64TextDecodeBs s of
        Left err -> fail err
        Right b  -> case S.decode b of
            Nothing     -> fail "Failed to S.decode SBox.Nonce"
            Just snonce -> return $ SBoxNonce snonce
    parseJSON _             = fail "SBoxNonce must be an Aeson String"

lock :: String -> ByteString -> IO Lockable
lock pwd b = do
    asalt <- ArgonSalt <$> getEntropy 32
    let aopts = defaultArgonOptions
    case S.decode $ deriveKey asalt aopts pwd of
        Nothing -> error "lockKey: Failed to decode derived master key"
        Just mk -> do
            snonce <- SBox.newNonce
            let sbox = SBox.secretbox mk snonce b
            return $ Argon2SBox asalt aopts (SBoxNonce snonce) sbox

deriveKey :: ArgonSalt -> ArgonOptions -> String -> ByteString
deriveKey (ArgonSalt asalt) (ArgonOptions hopts) pwd =
    hash hopts 32 (TE.encodeUtf8 $ T.pack pwd) asalt

unlock :: String -> Lockable -> Either String ByteString
unlock _   (Unlocked b)                                  = Right b
unlock pwd (Argon2SBox asalt aopts (SBoxNonce snonce) b) =
    case S.decode $ deriveKey asalt aopts pwd of
        Nothing -> Left "unlock: Failed to decode derived key"
        Just mk -> case SBox.secretboxOpen mk snonce b of
            Nothing   -> Left "unlock: Decryption failed. Wrong password?"
            Just decb -> Right decb

promptingUnlock :: Lockable -> IO (Either String ByteString)
promptingUnlock (Unlocked b) = return $ Right b
promptingUnlock locked       = runInputT defaultSettings prompt
  where
    prompt = do
        mpwd <- getPassword (Just '*') "Password: "
        case mpwd of
            Nothing  -> prompt
            Just ""  -> prompt
            Just pwd -> case unlock pwd locked of
                Left _  -> putStrLn "Invalid password. Try again." >> prompt
                Right b -> return $ Right b
