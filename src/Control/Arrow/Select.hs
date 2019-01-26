{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Arrow.Select where

import Control.Arrow
import Data.Foldable
import Data.Vector (Vector, fromListN)
import Data.Map (Map, toAscList, fromAscList)


class ArrowSelect f a where
  select :: a b c -> a (f b) (f c)


instance ArrowChoice a => ArrowSelect [] a where
  select = streamProcessor

instance ArrowChoice a => ArrowSelect Maybe a where
  select f = maybe (Left Nothing) Right ^>> right f >>^ either id Just

instance ArrowChoice a => ArrowSelect (Either k) a where
  select = right

instance ArrowChoice a => ArrowSelect Vector a where
  select f = arr length &&& arr toList >>> second (select f) >>^ uncurry fromListN

instance (ArrowChoice a, Eq k) => ArrowSelect (Map k) a where
  select f = toAscList ^>> unzip ^>> second (select f) >>^ uncurry zip >>^ fromAscList


streamProcessor :: ArrowChoice a => a b c -> a [b] [c]
streamProcessor a = proc xs -> case xs of
  b : bs -> do
    (c, cs) <- a *** streamProcessor a -< (b, bs)
    returnA -< c : cs
  _ -> returnA -< []
