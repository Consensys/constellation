{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Constellation.Node.Api where

import ClassyPrelude hiding (log)
import Control.Monad (void)
import Data.Aeson
    (FromJSON(parseJSON), ToJSON(toJSON), Value(Object), (.:), (.=), object)
import Data.Binary (encode, decodeOrFail)
import Data.Text.Format (Shown(Shown))
import qualified Data.Aeson as AE
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Network.Wai as Wai

import Constellation.Enclave.Payload
    (EncryptedPayload(EncryptedPayload, eplRcptBoxes), eplRcptBoxes)
import Constellation.Enclave.Types (PublicKey)
import Constellation.Node
import Constellation.Node.Types
import Constellation.Util.ByteString (mustB64TextDecodeBs)
import Constellation.Util.Logging (debugf, warnf)
import Constellation.Util.Wai
    (ok, badRequest, unauthorized, internalServerError)

data Send = Send
    { sreqPayload :: !ByteString
    , sreqFrom    :: !PublicKey
    , sreqTo      :: ![PublicKey]
    } deriving (Show)

instance FromJSON Send where
    parseJSON (Object v) = Send
        <$> (mustB64TextDecodeBs <$> v .: "payload")
        <*> v .: "from"
        <*> v .: "to"
    parseJSON _          = mzero

data SendResponse = SendResponse
    { sresKey :: !Text
    } deriving (Show)

instance ToJSON SendResponse where
    toJSON SendResponse{..} = object
        ["key" .= sresKey]

data Receive = Receive
    { rreqKey :: !Text
    , rreqTo  :: !PublicKey
    } deriving (Show)

instance FromJSON Receive where
    parseJSON (Object v) = Receive
        <$> v .: "key"
        <*> v .: "to"
    parseJSON _          = mzero

data ReceiveResponse = ReceiveResponse
    { rresPayload :: !ByteString
    } deriving (Show)

instance ToJSON ReceiveResponse where
    toJSON ReceiveResponse{..} = object
        [ "payload" .= TE.decodeUtf8 (B64.encode rresPayload)
        ]

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

data ApiRequest = ApiSend Send
                | ApiReceive Receive
                | ApiPush EncryptedPayload
                | ApiResend Resend
                | ApiPartyInfo PartyInfo
                | ApiUpcheck
                deriving (Show)

data ApiResponse = ApiSendR SendResponse
                 | ApiReceiveR ReceiveResponse
                 | ApiPushR Text
                 | ApiResendR ResendResponse
                 | ApiPartyInfoR PartyInfo
                 | ApiUpcheckR
                 deriving (Show)

app :: Bool -> TVar Node -> Wai.Application
app allowSendReceive nvar req resp = do
    b <- Wai.lazyRequestBody req
    let path = Wai.pathInfo req
    case parseRequest path b of
        Left err     -> do
            warnf "Failed to decode '{}' ({}) request: {}"
                ( TE.decodeUtf8 $ Wai.rawPathInfo req
                , TE.decodeUtf8 $ Wai.requestMethod req
                , err
                )
            resp badRequest
        Right apiReq -> if authenticateRequest allowSendReceive apiReq
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

parseRequest :: [Text] -> BL.ByteString -> Either String ApiRequest
-----
-- Node client
-----
parseRequest ["send"]      b = ApiSend <$> AE.eitherDecode' b
parseRequest ["receive"]   b = ApiReceive <$> AE.eitherDecode' b
-----
-- Node to node
-----
parseRequest ["push"]      b = case decodeOrFail b of
    Left  (_, _, err)  -> Left err
    Right (_, _, preq) -> Right $ ApiPush preq
parseRequest ["resend"]    b = ApiResend <$> AE.eitherDecode' b
parseRequest ["partyinfo"] b = case decodeOrFail b of
    Left  (_, _, err)   -> Left err
    Right (_, _, pireq) -> Right $ ApiPartyInfo pireq
-----
-- Miscellaneous
-----
parseRequest ["upcheck"]   _ = Right ApiUpcheck
parseRequest _             _ = Left "Not found"

authenticateRequest :: Bool -> ApiRequest -> Bool
authenticateRequest False (ApiSend _)    = False
authenticateRequest False (ApiReceive _) = False
authenticateRequest _     _              = True

performRequest :: TVar Node -> ApiRequest -> IO (Either String ApiResponse)
performRequest nvar (ApiSend sreq)       = readTVarIO nvar >>= \node -> fmap ApiSendR    <$> send node sreq
performRequest nvar (ApiReceive rreq)    = readTVarIO nvar >>= \node -> fmap ApiReceiveR <$> receive node rreq
performRequest nvar (ApiPush preq)       = readTVarIO nvar >>= \node -> fmap ApiPushR    <$> push node preq
performRequest nvar (ApiResend rreq)     = readTVarIO nvar >>= \node -> fmap ApiResendR  <$> resend node rreq
performRequest nvar (ApiPartyInfo pireq) = Right . ApiPartyInfoR <$> atomically (partyInfo nvar pireq)
performRequest _    ApiUpcheck           = return $ Right ApiUpcheckR

response :: ApiResponse -> BL.ByteString
response (ApiSendR sres)                        = AE.encode sres
response (ApiReceiveR rres)                     = AE.encode rres
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
