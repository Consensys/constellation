{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Logging where

import ClassyPrelude hiding (log)
import Control.Logging (log, log', warn, warn', debug, debug', errorL, errorL')
import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TFP

import Constellation.Util.Text (tformat)

logf, logf', warnf, warnf', debugf, debugf', errorf, errorf' ::
    TFP.Params ps => TF.Format -> ps -> IO ()
logf    fmt ps = log     $ tformat fmt ps
logf'   fmt ps = log'    $ tformat fmt ps
warnf   fmt ps = warn    $ tformat fmt ps
warnf'  fmt ps = warn'   $ tformat fmt ps
debugf  fmt ps = debug   $ tformat fmt ps
debugf' fmt ps = debug'  $ tformat fmt ps
errorf  fmt ps = errorL  $ tformat fmt ps
errorf' fmt ps = errorL' $ tformat fmt ps
