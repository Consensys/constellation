{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Constellation.Node.Config where

import ClassyPrelude
import Data.Aeson
    (FromJSON(parseJSON), Value(Object), (.:), (.:?), (.!=), toJSON, fromJSON)
import Text.Toml (parseTomlDoc)
import qualified Data.Aeson as AE
import qualified Data.Text.IO as TIO

import Constellation.Util.Exception (trys)

data Config = Config
    { cfgUrl                    :: !Text
    , cfgPort                   :: !Int
    , cfgSocketPath             :: !Text
    , cfgOtherNodeUrls          :: ![Text]
    , cfgPublicKeyPath          :: !FilePath
    , cfgPrivateKeyPath         :: !FilePath
    , cfgStoragePath            :: !String
    , cfgIpWhitelist            :: ![String]
    } deriving Show

instance FromJSON Config where
    parseJSON (Object v) = Config
        <$> v .:  "url"
        <*> v .:  "port"
        <*> v .:? "socketPath"    .!= "constellation.ipc"
        <*> v .:? "otherNodeUrls" .!= []
        <*> v .:  "publicKeyPath"
        <*> v .:  "privateKeyPath"
        <*> v .:  "storagePath"
        <*> v .:? "ipWhitelist"   .!= []
    parseJSON _          = mzero

loadConfigFile :: FilePath -> IO (Either String Config)
loadConfigFile fpath = do
    es <- trys $ TIO.readFile fpath
    return $ case es of
        Left err -> Left err
        Right s  -> loadConfig s

loadConfig :: Text -> Either String Config
loadConfig s = case parseTomlDoc "Error parsing TOML" s of
    Left err   -> Left $ show err
    Right toml -> case fromJSON $ toJSON toml of
        AE.Error err   -> Left err
        AE.Success cfg -> Right cfg
