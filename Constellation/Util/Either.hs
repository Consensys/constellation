{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Either where

import ClassyPrelude
import Control.Monad.Except (ExceptT(ExceptT))

fromRight :: Either a b -> b
fromRight (Left _)  = error "fromRight: Got Left"
fromRight (Right x) = x

fromShowRight :: Show a => Either a b -> b
fromShowRight (Left err) = error $ "fromShowRight: Got Left: " ++ show err
fromShowRight (Right x)  = x

flattenEithers :: Monoid a => a -> [Either a b] -> Either a [b]
flattenEithers sep es = case partitionEithers es of
    ([], rs) -> Right rs
    (ls, _)  -> Left $ mconcat $ intersperse sep ls

maybeToExceptT :: Monad m => e -> Maybe a -> ExceptT e m a
maybeToExceptT err = ExceptT . return . maybe (Left err) Right
