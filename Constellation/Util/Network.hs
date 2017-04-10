{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Network where

import ClassyPrelude
import Network.Socket ( Family(AF_INET), SocketType(Stream)
                      , SockAddr(SockAddrInet)
                      , aNY_PORT, iNADDR_ANY
                      , socket, socketPort, bind, close
                      )

getUnusedPort :: IO Int
getUnusedPort = do
    sock <- socket AF_INET Stream 0
    bind sock $ SockAddrInet aNY_PORT iNADDR_ANY
    port <- socketPort sock
    close sock
    return $ fromIntegral port
