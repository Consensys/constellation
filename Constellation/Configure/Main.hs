{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Configure.Main where

import ClassyPrelude
import System.Console.Haskeline
    (InputT, runInputT, defaultSettings)

defaultMain :: IO ()
defaultMain = runInputT defaultSettings configure

configure :: MonadIO m => InputT m ()
configure = putStrLn "The configuration tool hasn't been implemented yet"
