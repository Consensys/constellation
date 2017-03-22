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
    , cfgSocketPath      :: !Text
    , cfgOtherNodeUrls   :: ![Text]
    , cfgPublicKeyPaths  :: ![FilePath]
    , cfgPrivateKeyPaths :: ![FilePath]
    , cfgPasswordsPath   :: !FilePath
    , cfgStoragePath     :: !String
    , cfgIpWhitelist     :: ![String]
    , cfgJustShowVersion :: !Bool
    , cfgVerbose         :: !Bool
    } deriving Show

instance Default Config where
    def = Config
        { cfgUrl             = ""
        , cfgPort            = 0
        , cfgSocketPath      = "constellation.ipc"
        , cfgOtherNodeUrls   = []
        , cfgPublicKeyPaths  = []
        , cfgPrivateKeyPaths = []
        , cfgPasswordsPath   = ""
        , cfgStoragePath     = "storage"
        , cfgIpWhitelist     = []
        , cfgJustShowVersion = False
        , cfgVerbose         = False
        }

instance FromJSON Config where
    parseJSON (Object v) = undefined
    parseJSON _          = mzero

options :: [OptDescr (Config -> Config)]
options =
    [ Option [] ["url"] (OptArg (justDo setUrl) "URL")
      "URL for this node (what's advertised to other nodes, e.g. https://constellation.mydomain.com/)"

    , Option [] ["port"] (OptArg (justDo setPort) "PORT")
      "Port to listen on for the external API"

    , Option [] ["socket"] (OptArg (justDo setSocketPath) "FILE")
      "Path to IPC socket file to create for internal API access"

    , Option [] ["othernodeurls"] (OptArg (justDo setOtherNodeUrls) "URLs")
      "Comma-separated list of other node URLs to connect to on startup (this list may be incomplete)"

    , Option [] ["publickeys"] (OptArg (justDo setPublicKeyPaths) "FILE")
      "Comma-separated list of paths to public keys to advertise"

    , Option [] ["privatekeys"] (OptArg (justDo setPrivateKeyPaths) "FILE")
      "Comma-separated list of paths to corresponding private keys (these must be given in the same order as --publickeys)"

    , Option [] ["password"] (OptArg (justDo setPasswordsPath) "FILE")
      "A file containing the passwords for the specified --privatekeys, one per line, in the same order (if one key is not locked, add an empty line)"

    , Option [] ["storage"] (OptArg (justDo setStoragePath) "FILE")
      "Storage path to pass to the storage engine"

    , Option [] ["ipwhitelist"] (OptArg (justDo setIpWhitelist) "IPv4s/IPv6s")
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

setSocketPath :: String -> Config -> Config
setSocketPath s c = c { cfgSocketPath = T.pack s }

setOtherNodeUrls :: String -> Config -> Config
setOtherNodeUrls s c = c { cfgOtherNodeUrls = map (T.pack . trimBoth) (splitOn "," s) }

setPublicKeyPaths :: String -> Config -> Config
setPublicKeyPaths s c = c { cfgPublicKeyPaths = map trimBoth (splitOn "," s) }

setPrivateKeyPaths :: String -> Config -> Config
setPrivateKeyPaths s c = c { cfgPrivateKeyPaths = map trimBoth (splitOn "," s) }

setPasswordsPath :: String -> Config -> Config
setPasswordsPath s c = c { cfgPasswordsPath = s }

setStoragePath :: String -> Config -> Config
setStoragePath s c = c { cfgSocketPath = T.pack s }

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
