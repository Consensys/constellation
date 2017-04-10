{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Exception where

import ClassyPrelude
import Control.Exception (try)

trys :: IO a -> IO (Either String a)
trys f = someExceptionToStringEither <$> Control.Exception.try f

someExceptionToStringEither :: Either SomeException a -> Either String a
someExceptionToStringEither (Left e)  = Left $ show e
someExceptionToStringEither (Right x) = Right x
