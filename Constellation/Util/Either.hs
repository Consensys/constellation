{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Constellation.Util.Either where

import ClassyPrelude
import Control.Monad.Trans.Either (EitherT, hoistEither)

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

maybeToEitherT :: Monad m => e -> Maybe a -> EitherT e m a
maybeToEitherT err = hoistEither . maybe (Left err) Right
