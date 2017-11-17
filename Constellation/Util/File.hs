{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Constellation.Util.File where

import ClassyPrelude
import System.Directory (renameFile)
import System.Posix.Files (getFileStatus, fileMode, setFileCreationMask)
import System.Posix.Types (FileMode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Constellation.Util.Logging (warnf)

-- | Must run in the main thread
withOwnerReadWrite :: IO a -> IO a
withOwnerReadWrite = withFileCreationMask 127

-- | Must run in the main thread
withFileCreationMask :: FileMode -> IO a -> IO a
withFileCreationMask m f = do
    origm <- setFileCreationMask m
    x     <- f
    _     <- setFileCreationMask origm
    return x

worryAboutPermissions :: FilePath -> IO ()
worryAboutPermissions fpath = do
    mode <- fileMode <$> getFileStatus fpath
    -- Complain if mode isn't 0600
    when (mode /= 33152) $
        warnf "{} is too accessible. Please run: chmod 0600 {}" [fpath, fpath]

worriedReadFile :: FilePath -> IO ByteString
worriedReadFile fpath = do
    worryAboutPermissions fpath
    readFile fpath

writeFileBs :: FilePath -> ByteString -> IO ()
writeFileBs fpath b = B.writeFile tmp b >> renameFile tmp fpath
  where
    tmp = fpath <.> "tmp"

writeFileLbs :: FilePath -> BL.ByteString -> IO ()
writeFileLbs fpath b = BL.writeFile tmp b >> renameFile tmp fpath
  where
    tmp = fpath <.> "tmp"
