{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Constellation.Node.Config where

import ClassyPrelude
import Data.Aeson
    (FromJSON(parseJSON), Value(Object), (.:), (.:?), (.!=), toJSON, fromJSON)
import Data.Default (Default, def)
import System.Console.GetOpt (OptDescr, Option)
import System.Exit (exitSuccess)
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
    , cfgJustShowVersion        :: !Bool
    , cfgVerbose                :: !Bool
    } deriving Show

instance Default Config where
    def = Config
        { cfgUrl             = ""
        , cfgPort            = 0
        , cfgSocketPath      = "constellation.ipc"
        , cfgOtherNodeUrls   = []
        , cfgPublicKeyPath   = ""
        , cfgPrivateKeyPath  = ""
        , cfgStoragePath     = "storage"
        , cfgIpWhitelist     = []
        , cfgJustShowVersion = False
        , cfgVerbose         = False
        }

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

options :: [OptDescr (Config -> Config)]
options =
    [ Option [] ["url"] (OptArg $ maybe "" setUrl)
      "URL for this node (what's advertised to other nodes, e.g. https://constellation.mydomain.com/)"

    , Option [] ["port"] (OptArg $ maybe 0 setPort)
      "Port to listen on"

    , Option [] ["socketpath"] (OptArg setSocketPath)
      "Path to IPC socket file"

    , Option [] ["othernodeurls"] (OptArg setOtherNodeUrls)
      "(Possibly incomplete list of) other node URLs"

    , Option [] ["publickeypath"] (OptArg setPublicKeyPath)
      "Path to the public key to advertise"

    , Option [] ["privatekeypath"] (OptArg setPrivateKeyPath)
      "Path to the public key's corresponding private key"

    , Option [] ["storagepath"] (OptArg setStoragePath)
      "Storage path to pass to the storage engine"

    , Option [] ["ipwhitelist"] (OptArg setIpWhitelist)
        (OptArg (\c s -> 
        "Comma-separated list of IPv4 and IPv6 addresses that may connect to this node's external API"

    , Option ['v'] ["verbose"]
        (NoArg (\c -> c { cfgVerbose = True }))
        "print more detailed information"

    , Option ['V', '?'] ["version"]
        (NoArg (\c -> c { cfgJustShowVersion = True }))
    ]

setUrl :: String -> Config -> Config
setUrl Nothing  c = c
setUrl (Just s) c = c { cfgUrl = s }

setPort :: String -> Config -> Config
setPort Nothing c = c
setPort (Just s)  = c { cfgPort = read s }

setSocketPath :: String -> Config -> Config
setSocketPath Nothing c = c
setSocketPath (Just s)  = c { cfgSocketPath = s }

setOtherNodeUrls :: String -> Config -> Config
setOtherNodeUrls Nothing c = c
setOtherNodeUrls (Just s)  = c { cfgOtherNodeUrls = map trimBoth $ splitOn "," s }

setPublicKeyPath :: String -> Config -> Config
setPublicKeyPath Nothing c = c
setPublicKeyPath (Just s)  = c { cfgPublicKeyPath = s }

setPrivateKeyPath :: String -> Config -> Config
setPrivateKeyPath Nothing c = c
setPrivateKeyPath (Just s)  = c { cfgPrivateKeyPath = s }

setStoragePath :: String -> Config -> Config
setStoragePath Nothing c = c
setStoragePath (Just s)  = c { cfgSocketPath = s }

setIpWhitelist :: String -> Config -> Config
setIpWhitelist s = c { cfgIpWhitelist = map trimBoth $ splitOn "," s }

setVerbose :: Config -> Config
setVerbose = c { cfgVerbose = True }

setVersion :: Config -> Config
setVersion = c { cfgJustShowVersion = True }

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
