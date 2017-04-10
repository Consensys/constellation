{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module Constellation.Configure.Main where

import ClassyPrelude
import System.Console.Haskeline
    (InputT, runInputT, defaultSettings)

defaultMain :: IO ()
defaultMain = runInputT defaultSettings configure

configure :: MonadIO m => InputT m ()
configure = do
    putStrLn "hi"
