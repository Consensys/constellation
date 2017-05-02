{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Constellation.Node where

import ClassyPrelude
import Data.Binary (encode, decode)
import Network.HTTP.Conduit ( RequestBody(RequestBodyLBS)
                            , newManager, managerConnCount, tlsManagerSettings
                            , parseRequest, httpLbs, method, requestBody
                            , responseBody
                            )
import System.Random (StdGen, newStdGen, randomR)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Constellation.Enclave.Payload
    (EncryptedPayload(eplSender, eplRcptBoxes), eplRcptBoxes)
import Constellation.Enclave.Types (PublicKey(PublicKey, unPublicKey))
import Constellation.Node.Types
import Constellation.Util.Exception (trys)
import Constellation.Util.Logging (logf)

newNode :: Crypt
        -> Storage
        -> Text
        -> [PublicKey]
        -> [PublicKey]
        -> [Text]
        -> IO Node
newNode crypt storage url rcpts alwaysSendTo parties = do
    manager <- newManager tlsManagerSettings
        { managerConnCount = 100
        }
    return Node
        { nodePi           = PartyInfo
              { piUrl     = url
              , piRcpts   = HM.fromList $ map (\pub -> (pub, url)) rcpts
              , piParties = HS.fromList (url : parties)
              }
        , nodeCrypt        = crypt
        , nodeStorage      = storage
        , nodeAlwaysSendTo = alwaysSendTo
        , nodeManager      = manager
        }

runNode :: TVar Node -> IO ()
runNode nvar = do
    -- Aggressive synchronization while other nodes may be coming up (in an
    -- unknown order.)
    -- TODO: Smarter synchronization strategies
    let refreshThenDelay delay = nodeRefresh nvar >> threadDelay delay
    -- Every second for the first five seconds
    replicateM_ 5 (refreshThenDelay $ seconds 1)
    -- Add a small delay to ease barrage when spinning up many nodes at once
    g <- newStdGen
    let (jitter, ng) = randomR (1, 10) g
    threadDelay $ seconds jitter
    -- Every ten seconds for the next minute
    replicateM_ 6 (refreshThenDelay $ seconds 10)
    -- Every thirty seconds for the next two minutes
    replicateM_ 4 (refreshThenDelay $ seconds 30)
    -- Switch to normal synchronization
    refreshLoop ng nvar

seconds :: Int -> Int
seconds n = n * 1000 * 1000

refreshLoop :: StdGen -> TVar Node -> IO ()
refreshLoop g nvar = do
    nodeRefresh nvar
    -- Wait approximately five minutes
    let (jitteredDelay, ng) = randomR (280, 320) g
    threadDelay (seconds jitteredDelay) >> refreshLoop ng nvar

nodeRefresh :: TVar Node -> IO ()
nodeRefresh nvar = do
    node <- readTVarIO nvar
    let PartyInfo{..} = nodePi node
    epis <- mapM
        (getRemotePartyInfo nvar)
        (HS.toList $ HS.delete piUrl piParties)
    let pis = rights epis
    atomically $ mergePartyInfos nvar pis

getRemotePartyInfo :: TVar Node -> Text -> IO (Either String PartyInfo)
getRemotePartyInfo nvar url = trys $ do
    logf "Starting synchronization with {}" [url]
    Node{..} <- atomically $ readTVar nvar
    req      <- parseRequest $ T.unpack url ++ "partyinfo"
    let req' = req
            { method      = "POST"
            , requestBody = RequestBodyLBS $ encode nodePi
            }
    res <- httpLbs req' nodeManager
    logf "Finished synchronization with {}" [url]
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
sendPayload node@Node{..} pl from initRcpts = do
    let rcpts = nodeAlwaysSendTo ++ initRcpts
    eenc <- encryptPayload nodeCrypt pl from rcpts
    case eenc of
        Left err  -> return [Left err]
        Right epl -> do
            ek <- savePayload nodeStorage (epl, rcpts)
            case ek of
                Left err -> return [Left err]
                Right _  -> propagatePayload' node epl rcpts

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
        Left err         -> return $ Left err
        -- Rcpts is not set, meaning this is a payload sent to us, and we use
        -- the sender (not us) * to (us) to decrypt.
        Right (epl, [])  -> decryptPayload nodeCrypt epl to
        -- Rcpts is set, meaning this is a payload that we sent, and we use
        -- rcpt1 (not us) * the sender (us) to decrypt. (We pretend that the
        -- payload was actually sent by the first recipient to us, rather than
        -- the other way around, since NaCl doesn't care--the same shared key
        -- will be derived.)
        Right (epl, r:_) -> decryptPayload nodeCrypt epl'
            (PublicKey $ eplSender epl)
          where
            epl' = epl { eplSender = unPublicKey r }

-- TODO: Save written values and expunge when read once (since you typically
-- only need each payload once.)
onceCache :: Storage -> Storage
onceCache = id
