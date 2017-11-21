{-# LANGUAGE NoImplicitPrelude #-}
module Constellation.Util.String where

import ClassyPrelude hiding (splitAt)
import Prelude (splitAt)
import Data.Char (isSpace)

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . trimLeft . reverse

trimBoth :: String -> String
trimBoth = trimRight . trimLeft

truncateString :: Int -> String -> String
truncateString maxn s = if null rest then out else out ++ "..."
  where
    (out, rest) = splitAt (max 3 (maxn - 3)) s
