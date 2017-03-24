{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
module Constellation.Util.Logging where

import ClassyPrelude hiding (log)
import Control.Logging (log, log', warn, warn', debug, debug')
import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TFP

import Constellation.Util.Text (tformat)

logf, logf', warnf, warnf', debugf, debugf' :: TFP.Params ps => TF.Format -> ps -> IO ()
logf    fmt ps = log    $ tformat fmt ps
logf'   fmt ps = log'   $ tformat fmt ps
warnf   fmt ps = warn   $ tformat fmt ps
warnf'  fmt ps = warn'  $ tformat fmt ps
debugf  fmt ps = debug  $ tformat fmt ps
debugf' fmt ps = debug' $ tformat fmt ps
