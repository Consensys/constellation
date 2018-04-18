
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Configure.Main where

import Control.Monad.IO.Class
import System.Console.Haskeline  (InputT, runInputT, defaultSettings)

defaultMain :: IO ()
defaultMain = runInputT defaultSettings configure

configure :: MonadIO m => InputT m ()
configure = liftIO $ putStrLn "The configuration tool hasn't been implemented yet"
