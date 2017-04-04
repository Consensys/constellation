{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.AtExit where

import ClassyPrelude
import Control.Exception (SomeException)
import Control.Logging (warnS')
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T

funcs :: MVar (Maybe [IO ()])
funcs = unsafePerformIO (newMVar (Just []))
{-# NOINLINE funcs #-}

registerAtExit :: IO () -> IO ()
registerAtExit f = modifyMVar_ funcs $ \mhs ->
    case mhs of
        Just hs -> return $ Just (f:hs)
        Nothing -> return Nothing

withAtExit :: IO a -> IO a
withAtExit = bracket_ (return ()) exit
  where
    exit =  mask $ \unmask -> do
        Just fs <- swapMVar funcs Nothing
        forM_ fs $ \f -> catch (unmask f)
                         (\e -> warnS' "Failed to run an AtExit hook: "
                                (T.pack $ show (e :: SomeException))
                         )
