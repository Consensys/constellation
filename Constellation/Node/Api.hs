{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node.Api where

import ClassyPrelude hiding (delete, log)
import Control.Monad (void)
import Data.Aeson
    (FromJSON(parseJSON), ToJSON(toJSON), Value(Object), (.:), (.=), object)
import Data.Binary (encode, decodeOrFail)
import Data.HashMap.Strict ((!))
import Data.IP (IP(IPv4, IPv6), toHostAddress, toHostAddress6)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Text.Format (Shown(Shown))
import Network.HTTP.Types (Header, HeaderName, RequestHeaders)
import Network.HTTP.Types.Header (hContentLength)
import Network.Socket
    (SockAddr(SockAddrInet, SockAddrInet6), HostAddress, HostAddress6)
import Text.Read (read)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text.Encoding as TE
import qualified Network.Wai as Wai

import Constellation.Enclave.Payload
    (EncryptedPayload(EncryptedPayload, eplRcptBoxes), eplRcptBoxes)
import Constellation.Enclave.Types (PublicKey, mkPublicKey)
import Constellation.Node
import Constellation.Node.Types
import Constellation.Util.ByteString (mustB64DecodeBs, mustB64TextDecodeBs)
import Constellation.Util.Logging (debugf, warnf)
import Constellation.Util.Wai
    (ok, badRequest, unauthorized, internalServerError)

data Send = Send
    { sreqPayload :: ByteString
    , sreqFrom    :: PublicKey
    , sreqTo      :: [PublicKey]
    } deriving (Eq, Show)

instance FromJSON Send where
    parseJSON (Object v) = Send
        <$> (mustB64TextDecodeBs <$> v .: "payload")
        <*> v .: "from"
        <*> v .: "to"
    parseJSON _          = mzero

data SendResponse = SendResponse
    { sresKey :: Text
    } deriving (Show)

instance ToJSON SendResponse where
    toJSON SendResponse{..} = object
        ["key" .= sresKey]

data Receive = Receive
    { rreqKey :: Text
    , rreqTo  :: PublicKey
    } deriving (Show)

instance FromJSON Receive where
    parseJSON (Object v) = Receive
        <$> v .: "key"
        <*> v .: "to"
    parseJSON _          = mzero

data ReceiveResponse = ReceiveResponse
    { rresPayload :: ByteString
    } deriving (Show)

instance ToJSON ReceiveResponse where
    toJSON ReceiveResponse{..} = object
        [ "payload" .= TE.decodeUtf8 (B64.encode rresPayload)
        ]

data Delete = Delete
    { dreqKey :: Text
    } deriving (Show)

instance FromJSON Delete where
    parseJSON (Object v) = Delete
        <$> v .: "key"
    parseJSON _          = mzero

data DeleteResponse = DeleteResponse
    deriving (Show)

data Resend = ResendIndividual PublicKey Text
            | ResendAll PublicKey
            deriving (Show)

instance FromJSON Resend where
    parseJSON (Object v) = do
        t <- v .: "type"
        case (t :: Text) of
            "individual" -> ResendIndividual
                <$> v .: "publicKey"
                <*> v .: "key"
            "all"        -> ResendAll <$> v .: "publicKey"
            _            -> mzero
    parseJSON _          = mzero

data ResendResponse = ResendIndividualRes EncryptedPayload
                    | ResentAll
                    deriving (Show)

-- | The Private API allows sending, receiving/decrypting, deleting, and other
-- sensitive operations, while the Public API only allows what other nodes need
-- to exchange payloads.
data ApiType = Public
             | Private

data ApiRequest = ApiSend Send
                | ApiReceive Receive
                | ApiReceiveRaw Receive
                | ApiDelete Delete
                | ApiPush EncryptedPayload
                | ApiResend Resend
                | ApiPartyInfo PartyInfo
                | ApiUpcheck
                deriving (Show)

data ApiResponse = ApiSendR SendResponse
                 | ApiReceiveR ReceiveResponse
                 | ApiReceiveRawR ReceiveResponse
                 | ApiDeleteR DeleteResponse
                 | ApiPushR Text
                 | ApiResendR ResendResponse
                 | ApiPartyInfoR PartyInfo
                 | ApiUpcheckR
                 deriving (Show)

data Whitelist = Whitelist
    { wlIPv4 :: Set HostAddress
    , wlIPv6 :: Set HostAddress6
    } deriving Show

hFrom :: HeaderName
hFrom = "from"

hTo :: HeaderName
hTo = "to"

decodeSendRaw :: BL.ByteString -> RequestHeaders -> Either String Send
decodeSendRaw b h = case getHeaders [hContentLength, hFrom, hTo] h of
    Right headers -> Right Send
                    { sreqPayload = decodePayload (hmap ! hContentLength) b
                    , sreqFrom    = mustDecodeB64PublicKey $ hmap ! hFrom
                    , sreqTo      = decodePublicKeys $ hmap ! hTo
                    }
                    where hmap = HM.fromList headers
    Left err      -> Left err

decodePayload :: ByteString -> BL.ByteString -> ByteString
decodePayload h = toStrict . take (read $ BC.unpack h :: Int64)

mustDecodeB64PublicKey :: ByteString -> PublicKey
mustDecodeB64PublicKey = fromJust . mkPublicKey . mustB64DecodeBs

decodePublicKeys :: ByteString -> [PublicKey]
decodePublicKeys = (map mustDecodeB64PublicKey) . (BC.split ',')

getHeaders :: [HeaderName] -> RequestHeaders -> Either String RequestHeaders
getHeaders names headers =
    foldl' (\acc name -> case getHeader name headers of
               Just h  -> (case acc of
                             Right xs -> Right $ h:xs
                             err      -> err)
               Nothing -> Left $ "Missing header: " ++ show name
           ) (Right []) names

getHeader :: HeaderName -> RequestHeaders -> Maybe Header
getHeader hname headers =
    case (filter (\(header, _) -> header == hname) headers) of
        (x):_ -> Just x
        _     -> Nothing

whitelist :: [String] -> Whitelist
whitelist strs = Whitelist
    { wlIPv4 = Set.fromList v4Addrs
    , wlIPv6 = Set.fromList v6Addrs
    }
  where
    (v4Addrs, v6Addrs) = foldr f ([], []) strs
    f s (v4s, v6s)     = case read s of
        IPv4 addr4 -> (toHostAddress addr4 : v4s, v6s)
        IPv6 addr6 -> (v4s, toHostAddress6 addr6 : v6s)

whitelisted :: Whitelist -> SockAddr -> Bool
whitelisted Whitelist{..} (SockAddrInet _ addr)      = addr `Set.member` wlIPv4
whitelisted Whitelist{..} (SockAddrInet6 _ _ addr _) = addr `Set.member` wlIPv6
-- SockAddrUnix connects to the private API which has a Nothing whitelist
whitelisted _             _                          = False

app :: Maybe Whitelist -> ApiType -> TVar Node -> Wai.Application
app (Just wl) apiType nvar req resp =
    if whitelisted wl (Wai.remoteHost req)
        then request apiType nvar req resp
        else resp unauthorized
app Nothing   apiType nvar req resp =
    request apiType nvar req resp

request :: ApiType -> TVar Node -> Wai.Application
request apiType nvar req resp = do
    b <- Wai.lazyRequestBody req
    let h    = Wai.requestHeaders req
        path = Wai.pathInfo req
    case parseRequest path b h of
        Left err     -> do
            warnf "Failed to decode '{}' ({}) request: {}"
                ( TE.decodeUtf8 $ Wai.rawPathInfo req
                , TE.decodeUtf8 $ Wai.requestMethod req
                , err
                )
            resp badRequest
        Right apiReq -> if authorizedRequest apiType apiReq
            then do
                eapiRes <- performRequest nvar apiReq
                case eapiRes of
                    Left err     -> do
                        warnf "Error performing API request: {}; {}" (Shown apiReq, err)
                        resp internalServerError
                    Right apiRes -> do
                        debugf "Request from {}: {}; Response: {}"
                            ( Shown $ Wai.remoteHost req
                            , Shown apiReq
                            , Shown apiRes
                            )
                        resp $ ok $ response apiRes
            else do
                warnf "Blocked unauthorized request from {}: {}"
                    (Shown $ Wai.remoteHost req, Shown apiReq)
                resp unauthorized

parseRequest :: [Text] -> BL.ByteString -> RequestHeaders -> Either String ApiRequest
-----
-- Node client
-----
parseRequest ["send"]       b _ = ApiSend <$> AE.eitherDecode' b
parseRequest ["receive"]    b _ = ApiReceive <$> AE.eitherDecode' b
parseRequest ["sendRaw"]    b h = ApiSend <$> decodeSendRaw b h
parseRequest ["receiveRaw"] b _ = ApiReceiveRaw <$> AE.eitherDecode' b
parseRequest ["delete"]     b _ = ApiDelete <$> AE.eitherDecode' b
-----
-- Node to node
-----
parseRequest ["push"]       b _ = case decodeOrFail b of
    Left  (_, _, err)  -> Left err
    Right (_, _, preq) -> Right $ ApiPush preq
parseRequest ["resend"]     b _ = ApiResend <$> AE.eitherDecode' b
parseRequest ["partyinfo"]  b _ = case decodeOrFail b of
    Left  (_, _, err)   -> Left err
    Right (_, _, pireq) -> Right $ ApiPartyInfo pireq
-----
-- Miscellaneous
-----
parseRequest ["upcheck"]    _ _ = Right ApiUpcheck
parseRequest _              _ _ = Left "Not found"

authorizedRequest :: ApiType -> ApiRequest -> Bool
authorizedRequest Private (ApiSend _)       = True
authorizedRequest Private (ApiReceive _)    = True
authorizedRequest Private (ApiReceiveRaw _) = True
authorizedRequest Private (ApiDelete _)     = True
authorizedRequest _       (ApiPush _)       = True
authorizedRequest _       (ApiResend _)     = True
authorizedRequest _       (ApiPartyInfo _)  = True
authorizedRequest _       ApiUpcheck        = True
authorizedRequest _       _                 = False

performRequest :: TVar Node -> ApiRequest -> IO (Either String ApiResponse)
performRequest nvar (ApiSend sreq)       = readTVarIO nvar >>= \node -> fmap ApiSendR       <$> send node sreq
performRequest nvar (ApiReceive rreq)    = readTVarIO nvar >>= \node -> fmap ApiReceiveR    <$> receive node rreq
performRequest nvar (ApiReceiveRaw rreq) = readTVarIO nvar >>= \node -> fmap ApiReceiveRawR <$> receive node rreq
performRequest nvar (ApiDelete dreq)     = readTVarIO nvar >>= \node -> fmap ApiDeleteR     <$> delete node dreq
performRequest nvar (ApiPush preq)       = readTVarIO nvar >>= \node -> fmap ApiPushR       <$> push node preq
performRequest nvar (ApiResend rreq)     = readTVarIO nvar >>= \node -> fmap ApiResendR     <$> resend node rreq
performRequest nvar (ApiPartyInfo pireq) = Right . ApiPartyInfoR <$> atomically (partyInfo nvar pireq)
performRequest _    ApiUpcheck           = return $ Right ApiUpcheckR

response :: ApiResponse -> BL.ByteString
response (ApiSendR sres)                        = AE.encode sres
response (ApiReceiveR rres)                     = AE.encode rres
response (ApiReceiveRawR ReceiveResponse{..})   = BL.fromStrict rresPayload
response (ApiDeleteR DeleteResponse)            = ""
response (ApiPushR k)                           = BL.fromStrict $ TE.encodeUtf8 k
response (ApiResendR (ResendIndividualRes epl)) = encode epl
response (ApiResendR ResentAll)                 = ""
response (ApiPartyInfoR npi)                    = encode npi
response ApiUpcheckR                            = "I'm up!"

send :: Node -> Send -> IO (Either String SendResponse)
send node Send{..} = do
    -- TODO: This isn't great
    eks <- sendPayload node sreqPayload sreqFrom sreqTo
    let (ls, rs) = partitionEithers eks
    if null ls
        then case rs of
            (k:_) -> return $ Right SendResponse
                { sresKey = k
                }
            _     -> return $ Left $ "sendRequest: Got no keys back from sendPayload: " ++ show eks
        else return $ Left $ "sendRequest: Errors while running sendPayload: " ++ show eks

receive :: Node -> Receive -> IO (Either String ReceiveResponse)
receive node Receive{..} = do
    epl <- receivePayload node rreqKey rreqTo
    case epl of
        Left err -> return $ Left err
        Right pl -> return $ Right ReceiveResponse
            { rresPayload = pl
            }

delete :: Node -> Delete -> IO (Either String DeleteResponse)
delete Node{..} Delete{..} = do
    _ <- deletePayload nodeStorage dreqKey
    return $ Right DeleteResponse

push :: Node -> EncryptedPayload -> IO (Either String Text)
push Node{..} epl = savePayload nodeStorage (epl, [])

resend :: Node -> Resend -> IO (Either String ResendResponse)
resend Node{..}      (ResendIndividual pub k) = do
    epl <- loadPayload nodeStorage k
    case epl of
        Left _            -> return $ Left "Resend: Payload not found"
        Right (pl, rcpts) -> case plForPub pub pl rcpts of
            Just pl' -> return $ Right $ ResendIndividualRes pl'
            _        -> return $ Left "Resend: Payload is not for this recipient"
resend node@Node{..} (ResendAll pub)          = do
    -- TODO: This needs to be more async, e.g. return immediately
    -- here and ping the recipient with a callback when all payloads
    -- have been transferred.
    _ <- traverseStorage nodeStorage $ \_ (epl, rcpts) -> do
        _ <- case plForPub pub epl rcpts of
            -- TODO: Should this read TVar all the time or send all
            -- payloads to the same node, even if it changes?
            Just pl' -> void $ propagatePayload' node pl' [pub]
            Nothing  -> return ()
        return True
    return $ Right ResentAll

partyInfo :: TVar Node -> PartyInfo -> STM PartyInfo
partyInfo nvar pinfo = do
    mergePartyInfos nvar [pinfo]
    nodePi <$> readTVar nvar

plForPub :: PublicKey -> EncryptedPayload -> [PublicKey] -> Maybe EncryptedPayload
plForPub pub epl@EncryptedPayload{..} rcpts =
    case takeWhile ((pub ==) . fst) (zip rcpts eplRcptBoxes) of
        ((_, rcptBox):_) -> Just $ epl
            { eplRcptBoxes = [rcptBox]
            }
        _                -> Nothing
