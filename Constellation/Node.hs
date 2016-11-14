{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Constellation.Node where

import ClassyPrelude
import Data.Binary (encode, decode)
import Network.HTTP.Conduit ( RequestBody(RequestBodyLBS)
                            , newManager, managerConnCount, tlsManagerSettings
                            , parseRequest, httpLbs, method, requestBody
                            , responseBody
                            )
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Constellation.Enclave.Payload
    (EncryptedPayload(eplRcptBoxes), eplRcptBoxes)
import Constellation.Enclave.Types (PublicKey)
import Constellation.Node.Types
import Constellation.Util.Exception (trys)

newNode :: Crypt
        -> Storage
        -> Text
        -> PublicKey
        -> [PublicKey]
        -> [Text]
        -> IO Node
newNode crypt storage url archivalPub rcpts parties = do
    manager <- newManager tlsManagerSettings
        { managerConnCount = 100
        }
    return Node
        { nodePi                = PartyInfo
              { piUrl     = url
              , piRcpts   = HM.fromList $ map (\pub -> (pub, url)) rcpts
              , piParties = HS.fromList parties
              }
        , nodeCrypt             = crypt
        , nodeStorage           = storage
        , nodeManager           = manager
        , nodeArchivalPublicKey = archivalPub
        }

runNode :: TVar Node -> IO ()
runNode = refreshLoop

refreshLoop :: TVar Node -> IO ()
refreshLoop nvar = do
    nodeRefresh nvar
    threadDelay (5 * 60 * 1000 * 1000) >> refreshLoop nvar

nodeRefresh :: TVar Node -> IO ()
nodeRefresh nvar = do
    node <- readTVarIO nvar
    epis <- mapM
        (getRemotePartyInfo nvar)
        (HS.toList $ piParties $ nodePi node)
    let pis = rights epis
    atomically $ mergePartyInfos nvar pis

getRemotePartyInfo :: TVar Node -> Text -> IO (Either String PartyInfo)
getRemotePartyInfo nvar url = trys $ do
    Node{..} <- atomically $ readTVar nvar
    req      <- parseRequest $ T.unpack url ++ "partyinfo"
    let req' = req
            { method      = "POST"
            , requestBody = RequestBodyLBS $ encode nodePi
            }
    res <- httpLbs req' nodeManager
    return $ decode $ responseBody res

mergePartyInfos :: TVar Node -> [PartyInfo] -> STM ()
mergePartyInfos nvar pinfos = modifyTVar nvar $ \node ->
    -- TODO: Prevent the node from adding itself as a party. So meta.
    let pinfo = nodePi node
    in  node
            { nodePi = pinfo
              { piRcpts   = foldr (HM.union . piRcpts) (piRcpts pinfo) pinfos
              , piParties = foldr (HS.union . piParties) (piParties pinfo) pinfos
              }
            }

addParty :: Text -> TVar Node -> STM ()
addParty url nvar = modifyTVar nvar $ \node ->
    -- TODO: Lens is tempting...
    let curPi = nodePi node
    in  node
            { nodePi = curPi
              { piParties = HS.insert url (piParties curPi)
              }
            }

sendPayload :: Node
            -> ByteString
            -> PublicKey
            -> [PublicKey]
            -> IO [Either String Text]
sendPayload node@Node{..} pl from rcpts = do
    -- Add archival box for our retrieval purposes (can't encrypt to our own
    -- public key.)
    eenc <- encryptPayload nodeCrypt pl from (nodeArchivalPublicKey:rcpts)
    case eenc of
        Left err  -> return [Left err]
        Right epl -> do
            ek <- savePayload nodeStorage (epl, rcpts)
            case ek of
                Left err -> return [Left err]
                Right _  ->
                    -- Remove the archival box before propagating
                    let epl' = epl { eplRcptBoxes = drop 1 $ eplRcptBoxes epl }
                     in propagatePayload' node epl' rcpts

propagatePayload :: TVar Node
                 -> EncryptedPayload
                 -> [PublicKey]
                 -> IO [Either String Text]
propagatePayload nvar epl rcpts = atomically (readTVar nvar) >>= \node ->
    propagatePayload' node epl rcpts

propagatePayload' :: Node
                  -> EncryptedPayload
                  -> [PublicKey]
                  -> IO [Either String Text]
propagatePayload' Node{..} epl rcpts =
    mapConcurrently f $ zip rcpts (eplRcptBoxes epl)
  where
    -- TODO: Smarter grouping of requests
    f (pub, rcptBox) = trys $ case HM.lookup pub (piRcpts nodePi) of
        Nothing  -> error "Unknown recipient"
        Just url -> do
            req <- parseRequest $ T.unpack url ++ "push"
            let req' = req
                    { method      = "POST"
                    , requestBody = RequestBodyLBS $ encode epl
                          { eplRcptBoxes = [rcptBox]
                          }
                    }
            res <- httpLbs req' nodeManager
            return $ TE.decodeUtf8 $ BL.toStrict $ responseBody res

receivePayload :: Node -> Text -> PublicKey -> IO (Either String ByteString)
receivePayload Node{..} key to = do
    eepl <- loadPayload nodeStorage key
    case eepl of
        Left err           -> return $ Left err
        Right (epl, rcpts) -> if null rcpts
            -- Rcpts is not set, meaning this is a payload sent to us, and
            -- we use the expected key to decrypt.
            then decryptPayload nodeCrypt epl to
            -- Rcpts is set, meaning this is a payload that we sent, and we
            -- use the archival key to decrypt.
            else decryptPayload nodeCrypt epl nodeArchivalPublicKey

-- TODO: Save written values and expunge when read once (since you typically
-- only need each payload once.)
onceCache :: Storage -> Storage
onceCache = id
