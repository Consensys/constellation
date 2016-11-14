{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Constellation.Util.Text where

import ClassyPrelude

import qualified Data.Text.Format as TF
import qualified Data.Text.Format.Params as TFP
import qualified Data.Text.Lazy as TL

tformat :: TFP.Params ps => TF.Format -> ps -> Text
tformat format params = TL.toStrict $ TF.format format params
