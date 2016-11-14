{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Constellation.Configure.Main where

import ClassyPrelude
import System.Console.Haskeline
    (InputT, runInputT, defaultSettings, getPassword)
import System.Environment (getArgs, getProgName)
import Text.Printf (printf)

defaultMain :: IO ()
defaultMain = runInputT defaultSettings configure

configure :: MonadIO m => InputT m ()
configure = do
    putStrLn "hi"
