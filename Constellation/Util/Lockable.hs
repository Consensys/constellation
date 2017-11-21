{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Lockable where

import ClassyPrelude hiding (hash)
import Crypto.Error (CryptoFailable(..))
import Crypto.KDF.Argon2 (Options(..), Variant(..), Version(..), hash)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), (.:), (.:?), (.=), object)
import System.Console.Haskeline (runInputT, defaultSettings, getPassword)
import System.Entropy (getEntropy)
import qualified Data.Aeson as AE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Crypto.Saltine.Class as S
import qualified Crypto.Saltine.Core.SecretBox as SBox

import Constellation.Util.ByteString (b64TextEncodeBs, b64TextDecodeBs)

defaultArgonOptions :: ArgonOptions
defaultArgonOptions = ArgonOptions Options
    { iterations  = 10
    , memory      = (2 :: Word32) ^ (20 :: Word32)  -- 1 GiB
    , parallelism = 4
    , variant     = Argon2id
    , version     = Version13
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

newtype ArgonOptions = ArgonOptions { unArgonOptions :: Options }

instance Show ArgonOptions where
    show (ArgonOptions options) = show options

instance Eq ArgonOptions where
    (==) (ArgonOptions options) (ArgonOptions options') = options == options'

instance ToJSON ArgonOptions where
    toJSON (ArgonOptions Options{..}) = object
        [ "iterations"  .= iterations
        , "memory"      .= memory
        , "parallelism" .= parallelism
        , "variant"     .= case variant of
              Argon2i  -> "i"  :: Text
              Argon2d  -> "d"  :: Text
              Argon2id -> "id" :: Text
        , "version"     .= case version of
              Version13 -> "1.3" :: Text
              Version10 -> "1.0" :: Text
        ]

instance FromJSON ArgonOptions where
    parseJSON (AE.Object v) = do
        iterations  <- v .:  "iterations"
        memory      <- v .:  "memory"
        parallelism <- v .:  "parallelism"
        varStr      <- v .:  "variant"
        verStr      <- v .:? "version"
        variant     <- case varStr :: Text of
            "i"  -> return Argon2i
            "d"  -> return Argon2d
            "id" -> return Argon2id
            _    -> fail "Unrecognized Argon2 variant"
        version     <- case verStr :: Maybe Text of
            Just "1.3" -> return Version13
            Just "1.0" -> return Version10
            _          -> return Version13
        return $ ArgonOptions Options{..}
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
    case hash hopts (TE.encodeUtf8 $ T.pack pwd) asalt 32 of
        CryptoPassed a -> a
        CryptoFailed e -> error $ "deriveKey failed: " ++ show e

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
