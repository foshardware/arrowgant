{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Control.Arrow.Select where

import Control.Arrow
import Control.Lens
import Data.Foldable
import Data.Vector (Vector, fromListN)
import Data.Map (Map, toAscList, fromAscList)



hierarchical :: (Arrow a, ArrowSelect f a) => Lens b b (f b) (f b) -> a b b -> a b b
hierarchical descend act = proc b -> do
  models <- hierarchical descend act & select -< b ^. descend
  act -< b & descend .~ models


class ArrowSelect f a where
  select :: a b c -> a (f b) (f c)


instance Arrow a => ArrowSelect ((,) b) a where
  select = second

instance ArrowChoice a => ArrowSelect [] a where
  select = streamProcessor

instance ArrowChoice a => ArrowSelect Maybe a where
  select f = maybe (Left Nothing) Right ^>> right f >>^ either id Just

instance ArrowChoice a => ArrowSelect (Either b) a where
  select = right

instance ArrowChoice a => ArrowSelect Vector a where
  select f = arr length &&& arr toList >>> second (select f) >>^ uncurry fromListN

instance (ArrowChoice a, Eq b) => ArrowSelect (Map b) a where
  select f = toAscList ^>> unzip ^>> second (select f) >>^ uncurry zip >>^ fromAscList


streamProcessor :: ArrowChoice a => a b c -> a [b] [c]
streamProcessor a = proc xs -> case xs of
  b : bs -> do
    (c, cs) <- a *** streamProcessor a -< (b, bs)
    returnA -< c : cs
  _ -> returnA -< []
