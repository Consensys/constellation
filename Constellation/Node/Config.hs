{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Constellation.Node.Config where

import ClassyPrelude
import Data.Aeson
    (FromJSON(parseJSON), Value(Object), (.:), (.:?), (.!=), toJSON, fromJSON)
import Data.Default (Default, def)
import Data.List.Split (splitOn)
import System.Console.GetOpt
    ( OptDescr(Option), ArgOrder(Permute), ArgDescr(NoArg, OptArg)
    , getOpt, usageInfo
    )
import System.Exit (exitSuccess)
import Text.Read (read)
import Text.Toml (parseTomlDoc)
import qualified Data.Aeson as AE
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Constellation.Util.Exception (trys)
import Constellation.Util.String (trimBoth)

data Config = Config
    { cfgUrl             :: !Text
    , cfgPort            :: !Int
    , cfgSocket          :: !(Maybe FilePath)
    , cfgOtherNodes      :: ![Text]
    , cfgPublicKeys      :: ![FilePath]
    , cfgPrivateKeys     :: ![FilePath]
    , cfgPasswords       :: !(Maybe FilePath)
    , cfgStorage         :: !String
    , cfgIpWhitelist     :: ![String]
    , cfgJustShowVersion :: !Bool
    , cfgVerbose         :: !Bool  -- another level for debug?
    } deriving Show

instance Default Config where
    def = Config
        { cfgUrl             = ""
        , cfgPort            = 0
        , cfgSocket          = Nothing
        , cfgOtherNodes      = []
        , cfgPublicKeys      = []
        , cfgPrivateKeys     = []
        , cfgPasswords       = Nothing
        , cfgStorage         = "storage"
        , cfgIpWhitelist     = []
        , cfgJustShowVersion = False
        , cfgVerbose         = False
        }

instance FromJSON Config where
    parseJSON (Object v) = Config
        <$> v .:? "url"         .!= ""
        <*> v .:? "port"        .!= 0
        <*> v .:? "socket"
        <*> v .:? "otherNodes"  .!= []
        <*> v .:? "publicKeys"  .!= []
        <*> v .:? "privateKeys" .!= []
        <*> v .:? "passwords"
        <*> v .:? "storage"     .!= "storage"
        <*> v .:? "ipWhitelist" .!= []
        <*> pure False
        <*> v .:? "verbose"     .!= False
    parseJSON _          = mzero

options :: [OptDescr (Config -> Config)]
options =
    [ Option [] ["url"] (OptArg (justDo setUrl) "URL")
      "URL for this node (what's advertised to other nodes, e.g. https://constellation.mydomain.com/)"

    , Option [] ["port"] (OptArg (justDo setPort) "PORT")
      "Port to listen on for the external API"

    , Option [] ["socket"] (OptArg (justDo setSocket) "FILE")
      "Path to IPC socket file to create for internal API access"

    , Option [] ["otherNodes"] (OptArg (justDo setOtherNodes) "URLs")
      "Comma-separated list of other node URLs to connect to on startup (this list may be incomplete)"

    , Option [] ["publicKeys"] (OptArg (justDo setPublicKeys) "FILE")
      "Comma-separated list of paths to public keys to advertise"

    , Option [] ["privateKeys"] (OptArg (justDo setPrivateKeys) "FILE")
      "Comma-separated list of paths to corresponding private keys (these must be given in the same order as --publickeys)"

    , Option [] ["password"] (OptArg (justDo setPasswords) "FILE")
      "A file containing the passwords for the specified --privatekeys, one per line, in the same order (if one key is not locked, add an empty line)"

    , Option [] ["storage"] (OptArg (justDo setStorage) "FILE")
      "Storage path to pass to the storage engine"

    , Option [] ["ipWhitelist"] (OptArg (justDo setIpWhitelist) "IPv4s/IPv6s")
      "Comma-separated list of IPv4 and IPv6 addresses that may connect to this node's external API"

    , Option ['v'] ["verbose"] (NoArg setVerbose)
      "print more detailed information"

    , Option ['V', '?'] ["version"] (NoArg setVersion)
      "output current version information and exit"
    ]

justDo :: (String -> Config -> Config) -> Maybe String -> Config -> Config
justDo _ Nothing  c = c
justDo f (Just s) c = f s c

setUrl :: String -> Config -> Config
setUrl s c = c { cfgUrl = T.pack s }

setPort :: String -> Config -> Config
setPort s c = c { cfgPort = read s }

setSocket :: String -> Config -> Config
setSocket s c = c { cfgSocket = Just s }

setOtherNodes :: String -> Config -> Config
setOtherNodes s c = c { cfgOtherNodes = map (T.pack . trimBoth) (splitOn "," s) }

setPublicKeys :: String -> Config -> Config
setPublicKeys s c = c { cfgPublicKeys = map trimBoth (splitOn "," s) }

setPrivateKeys :: String -> Config -> Config
setPrivateKeys s c = c { cfgPrivateKeys = map trimBoth (splitOn "," s) }

setPasswords :: String -> Config -> Config
setPasswords s c = c { cfgPasswords = Just s }

setStorage :: String -> Config -> Config
setStorage s c = c { cfgStorage = s }

setIpWhitelist :: String -> Config -> Config
setIpWhitelist s c = c { cfgIpWhitelist = map trimBoth (splitOn "," s) }

setVerbose :: Config -> Config
setVerbose c = c { cfgVerbose = True }

setVersion :: Config -> Config
setVersion c = c { cfgJustShowVersion = True }

extractConfig :: [String] -> IO (Config, [String])
extractConfig []   = errorOut ""
extractConfig argv = case getOpt Permute options argv of
    (o, n, [])   -> do
        initCfg <- case n of
            []        -> def
            [cfgPath] -> loadConfigFile cfgPath >>= \case
                Left err  -> errorOut ("Failed to load configuration file " ++ cfgPath ++ ": " ++ err)
                Right cfg -> return cfg
            _         -> errorOut "Only one configuration file can be specified"
        return (foldl' (flip id) initCfg o, n)
    (_, _, errs) -> errorOut (concat errs)

-- errorOut :: String -> a
errorOut s = ioError (userError $ s ++ usageInfo header options) >> undefined
  where
    header = "Usage: constellation-node [OPTION...] [config file containing options]\n(If a configuration file is specified, any command line options will take precedence.)"

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
