{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Config where

import ClassyPrelude
import Data.Aeson
    (FromJSON(parseJSON), Value(Object), (.:?), (.!=), toJSON, fromJSON)
import Data.Default (Default, def)
import Data.List.Split (splitOn)
import System.Console.GetOpt
    ( OptDescr(Option), ArgOrder(Permute), ArgDescr(NoArg, OptArg)
    , getOpt, usageInfo
    )
import Text.Read (read)
import Text.Toml (parseTomlDoc)
import qualified Data.Aeson as AE
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Constellation.Util.Exception (trys)
import Constellation.Util.String (trimBoth)

data Config = Config
    { cfgUrl              :: Text
    , cfgPort             :: Int
    , cfgWorkDir          :: Maybe FilePath
    , cfgSocket           :: Maybe FilePath
    , cfgOtherNodes       :: [Text]
    , cfgPublicKeys       :: [FilePath]
    , cfgPrivateKeys      :: [FilePath]
    , cfgAlwaysSendTo     :: [FilePath]
    , cfgPasswords        :: Maybe FilePath
    , cfgStorage          :: String
    , cfgIpWhitelist      :: [String]
    , cfgTls              :: String
    , cfgTlsServerCert    :: FilePath
    , cfgTlsServerChain   :: [FilePath]
    , cfgTlsServerKey     :: FilePath
    , cfgTlsServerTrust   :: String
    , cfgTlsKnownClients  :: FilePath
    , cfgTlsClientCert    :: FilePath
    , cfgTlsClientChain   :: [FilePath]
    , cfgTlsClientKey     :: FilePath
    , cfgTlsClientTrust   :: String
    , cfgTlsKnownServers  :: FilePath
    , cfgJustShowVersion  :: Bool
    , cfgJustGenerateKeys :: [String]
    , cfgVerbosity        :: Int
    } deriving Show

instance Default Config where
    def = Config
        { cfgUrl              = ""
        , cfgPort             = 0
        , cfgWorkDir          = Nothing
        , cfgSocket           = Nothing
        , cfgOtherNodes       = []
        , cfgPublicKeys       = []
        , cfgPrivateKeys      = []
        , cfgAlwaysSendTo     = []
        , cfgPasswords        = Nothing
        , cfgStorage          = "dir:storage"
        , cfgIpWhitelist      = []
        , cfgTls              = "strict"
        , cfgTlsServerCert    = "tls-server-cert.pem"
        , cfgTlsServerChain   = []
        , cfgTlsServerKey     = "tls-server-key.pem"
        , cfgTlsServerTrust   = "ca-or-tofu"
        , cfgTlsKnownClients  = "tls-known-clients"
        , cfgTlsClientCert    = "tls-client-cert.pem"
        , cfgTlsClientChain   = []
        , cfgTlsClientKey     = "tls-client-key.pem"
        , cfgTlsClientTrust   = "ca-or-tofu"
        , cfgTlsKnownServers  = "tls-known-servers"
        , cfgJustShowVersion  = False
        , cfgJustGenerateKeys = []
        , cfgVerbosity        = defaultVerbosity
        }

instance FromJSON Config where
    -- JSON instance for conversion from TOML
    parseJSON (Object v) = do
        -- DEPRECATE: Backwards compatibility with v0.0.1 config format
        moldPrivPath <- v .:? "privateKeyPath"
        case moldPrivPath of
            Just oldPrivPath -> do
                cfg        <- parse
                msocket    <- v .:? "socketPath"
                otherNodes <- v .:? "otherNodeUrls" .!= []
                pubKeys    <- maybeToList <$> v .:? "publicKeyPath"
                apubs      <- maybeToList <$> v .:? "archivalPublicKeyPath"
                aprivs     <- maybeToList <$> v .:? "archivalPrivateKeyPath"
                storage    <- v .:? "storagePath" .!= "storage"
                ipwl       <- v .:? "ipWhitelist" .!= []
                return cfg
                    { cfgSocket       = msocket
                    , cfgOtherNodes   = otherNodes
                    , cfgPublicKeys   = pubKeys ++ apubs
                    , cfgPrivateKeys  = oldPrivPath : aprivs
                    , cfgAlwaysSendTo = apubs
                    , cfgStorage      = storage
                    , cfgIpWhitelist  = ipwl
                    }
            Nothing        -> parse
      where
        parse = Config
            <$> v .:? "url"             .!= cfgUrl def
            <*> v .:? "port"            .!= cfgPort def
            <*> v .:? "workdir"         .!= cfgWorkDir def
            <*> v .:? "socket"          .!= cfgSocket def
            <*> v .:? "othernodes"      .!= cfgOtherNodes def
            <*> v .:? "publickeys"      .!= cfgPublicKeys def
            <*> v .:? "privatekeys"     .!= cfgPrivateKeys def
            <*> v .:? "alwayssendto"    .!= cfgAlwaysSendTo def
            <*> v .:? "passwords"       .!= cfgPasswords def
            <*> v .:? "storage"         .!= cfgStorage def
            <*> v .:? "ipwhitelist"     .!= cfgIpWhitelist def
            <*> v .:? "tls"             .!= cfgTls def
            <*> v .:? "tlsservercert"   .!= cfgTlsServerCert def
            <*> v .:? "tlsserverchain"  .!= cfgTlsServerChain def
            <*> v .:? "tlsserverkey"    .!= cfgTlsServerKey def
            <*> v .:? "tlsservertrust"  .!= cfgTlsServerTrust def
            <*> v .:? "tlsknownclients" .!= cfgTlsKnownClients def
            <*> v .:? "tlsclientcert"   .!= cfgTlsClientCert def
            <*> v .:? "tlsclientchain"  .!= cfgTlsClientChain def
            <*> v .:? "tlsclientkey"    .!= cfgTlsClientKey def
            <*> v .:? "tlsclienttrust"  .!= cfgTlsClientTrust def
            <*> v .:? "tlsknownservers" .!= cfgTlsKnownServers def
            <*> pure (cfgJustShowVersion def)
            <*> pure (cfgJustGenerateKeys def)
            <*> v .:? "verbosity"       .!= cfgVerbosity def
    parseJSON _          = mzero

defaultVerbosity :: Int
defaultVerbosity = 1  -- Warnings by default

options :: [OptDescr (Config -> Config)]
options =
    [ Option [] ["url"] (OptArg (justDo setUrl) "URL")
      "URL for this node (what's advertised to other nodes, e.g. https://constellation.mydomain.com/)"

    , Option [] ["port"] (OptArg (justDo setPort) "NUM")
      "Port to listen on for the public API"

    , Option [] ["workdir"] (OptArg (justDo setWorkDir) "DIR")
      "Working directory to use (relative paths specified for other options are relative to the working directory)"

    , Option [] ["socket"] (OptArg (justDo setSocket) "FILE")
      "Path to IPC socket file to create for private API access"

    , Option [] ["othernodes"] (OptArg (justDo setOtherNodes) "URL...")
      "Comma-separated list of other node URLs to connect to on startup (this list may be incomplete)"

    , Option [] ["publickeys"] (OptArg (justDo setPublicKeys) "FILE...")
      "Comma-separated list of paths to public keys to advertise"

    , Option [] ["privatekeys"] (OptArg (justDo setPrivateKeys) "FILE...")
      "Comma-separated list of paths to corresponding private keys (these must be given in the same order as --publickeys)"

    , Option [] ["alwayssendto"] (OptArg (justDo setAlwaysSendTo) "FILE...")
      "Comma-separated list of paths to public keys that are always included as recipients (these must be advertised somewhere)"

    , Option [] ["passwords"] (OptArg (justDo setPasswords) "FILE")
      "A file containing the passwords for the specified --privatekeys, one per line, in the same order (if one key is not locked, add an empty line)"

    , Option [] ["storage"] (OptArg (justDo setStorage) "STRING")
      "Storage string specifying a storage engine and/or storage path"

    , Option [] ["ipwhitelist"] (OptArg (justDo setIpWhitelist) "IP...")
      "Comma-separated list of IPv4 and IPv6 addresses that may connect to this node's public API"

    , Option [] ["tls"] (OptArg (justDo setTls) "STRING")
      "TLS status (strict, off)"

    , Option [] ["tlsservercert"] (OptArg (justDo setTlsServerCert) "FILE")
      "TLS certificate file to use for the public API"

    , Option [] ["tlsserverchain"] (OptArg (justDo setTlsServerChain) "FILE...")
      "Comma separated list of TLS chain certificates to use for the public API"

    , Option [] ["tlsserverkey"] (OptArg (justDo setTlsServerKey) "FILE")
      "TLS key to use for the public API"

    , Option [] ["tlsservertrust"] (OptArg (justDo setTlsServerTrust) "STRING")
      "TLS server trust mode (whitelist, ca-or-tofu, ca, tofu, insecure-no-validation)"

    , Option [] ["tlsknownclients"] (OptArg (justDo setTlsKnownClients) "FILE")
      "TLS server known clients file for the ca-or-tofu, tofu and whitelist trust modes"

    , Option [] ["tlsclientcert"] (OptArg (justDo setTlsClientCert) "FILE")
      "TLS client certificate file to use for connections to other nodes"

    , Option [] ["tlsclientchain"] (OptArg (justDo setTlsClientChain) "FILE...")
      "Comma separated list of TLS chain certificates to use for connections to other nodes"

    , Option [] ["tlsclientkey"] (OptArg (justDo setTlsClientKey) "FILE")
      "TLS key to use for connections to other nodes"

    , Option [] ["tlsclienttrust"] (OptArg (justDo setTlsClientTrust) "STRING")
      "TLS client trust mode (whitelist, ca-or-tofu, ca, tofu, insecure-no-validation)"

    , Option [] ["tlsknownservers"] (OptArg (justDo setTlsKnownServers) "FILE")
      "TLS client known servers file for the ca-or-tofu, tofu and whitelist trust modes"

    , Option ['v'] ["verbosity"] (OptArg setVerbosity "NUM")
      "Print more detailed information (optionally specify a number or add v's to increase verbosity)"

    , Option ['V', '?'] ["version"] (NoArg setVersion)
      "Output current version information, then exit"

    , Option [] ["generatekeys"] (OptArg (justDo setGenerateKeys) "NAME...")
      "Comma-separated list of key pair names to generate, then exit"
    ]

justDo :: (String -> Config -> Config) -> Maybe String -> Config -> Config
justDo _ Nothing  c = c
justDo f (Just s) c = f s c

setUrl :: String -> Config -> Config
setUrl s c = c { cfgUrl = T.pack s }

setPort :: String -> Config -> Config
setPort s c = c { cfgPort = read s }

setWorkDir :: String -> Config -> Config
setWorkDir s c = c { cfgWorkDir = Just s }

setSocket :: String -> Config -> Config
setSocket s c = c { cfgSocket = Just s }

setOtherNodes :: String -> Config -> Config
setOtherNodes s c = c { cfgOtherNodes = map (T.pack . trimBoth) (splitOn "," s) }

setPublicKeys :: String -> Config -> Config
setPublicKeys s c = c { cfgPublicKeys = map trimBoth (splitOn "," s) }

setPrivateKeys :: String -> Config -> Config
setPrivateKeys s c = c { cfgPrivateKeys = map trimBoth (splitOn "," s) }

setAlwaysSendTo :: String -> Config -> Config
setAlwaysSendTo s c = c { cfgAlwaysSendTo = map trimBoth (splitOn "," s) }

setPasswords :: String -> Config -> Config
setPasswords s c = c { cfgPasswords = Just s }

setStorage :: String -> Config -> Config
setStorage s c = c { cfgStorage = s }

setIpWhitelist :: String -> Config -> Config
setIpWhitelist s c = c { cfgIpWhitelist = map trimBoth (splitOn "," s) }

setTls :: String -> Config -> Config
setTls s c = c { cfgTls = s }

setTlsServerCert :: String -> Config -> Config
setTlsServerCert s c = c { cfgTlsServerCert = s }

setTlsServerChain :: String -> Config -> Config
setTlsServerChain s c = c { cfgTlsServerChain = map trimBoth (splitOn "," s) }

setTlsServerKey :: String -> Config -> Config
setTlsServerKey s c = c { cfgTlsServerKey = s }

setTlsServerTrust :: String -> Config -> Config
setTlsServerTrust s c = c { cfgTlsServerTrust = s }

setTlsKnownClients :: String -> Config -> Config
setTlsKnownClients s c = c { cfgTlsKnownClients = s }

setTlsClientCert :: String -> Config -> Config
setTlsClientCert s c = c { cfgTlsClientCert = s }

setTlsClientChain :: String -> Config -> Config
setTlsClientChain s c = c { cfgTlsClientChain = map trimBoth (splitOn "," s) }

setTlsClientKey :: String -> Config -> Config
setTlsClientKey s c = c { cfgTlsClientKey = s }

setTlsClientTrust :: String -> Config -> Config
setTlsClientTrust s c = c { cfgTlsClientTrust = s }

setTlsKnownServers :: String -> Config -> Config
setTlsKnownServers s c = c { cfgTlsKnownClients = s }

setVerbosity :: Maybe String -> Config -> Config
setVerbosity (Just s) c = c { cfgVerbosity = n }
  where
    n = if all (== 'v') s
            then 2 + length s  -- -v+(length s) v's
            else read s  -- assume a number was given
setVerbosity Nothing  c = c { cfgVerbosity = 2 }  -- '-v' given, increase to info

setVersion :: Config -> Config
setVersion c = c { cfgJustShowVersion = True }

setGenerateKeys :: String -> Config -> Config
setGenerateKeys s c = c { cfgJustGenerateKeys = map trimBoth (splitOn "," s) }

extractConfig :: [String] -> IO (Config, [String])
extractConfig []   = errorOut "No arguments given"
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

errorOut :: String -> IO a
errorOut s = ioError (userError $ s ++ ". " ++ usageInfo header options) >> error ""
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
