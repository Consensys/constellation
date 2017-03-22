{-# LANGUAGE NoImplicitPrelude #-}
module Constellation.Util.String where

import ClassyPrelude
import Data.Char (isSpace)

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . trimLeft . reverse

trimBoth :: String -> String
trimBoth = trimRight . trimLeft
