{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Arrow.StreamProcessor where

import Control.Arrow
import Data.Foldable
import Data.Maybe
import Data.Vector (Vector, fromListN)
import Data.Map (Map, toAscList, fromAscList)
import Prelude hiding (id, (.))


class ArrowSelect f a where
  select :: a b c -> a (f b) (f c)


instance ArrowChoice a => ArrowSelect [] a where
  select = streamProcessor

instance ArrowChoice a => ArrowSelect Maybe a where
  select f = maybeToList ^>> streamProcessor f >>^ listToMaybe

instance ArrowChoice a => ArrowSelect (Either k) a where
  select f = right f

instance ArrowChoice a => ArrowSelect Vector a where
  select f = arr length &&& arr toList >>> second (streamProcessor f) >>^ uncurry fromListN

instance (ArrowChoice a, Eq k) => ArrowSelect (Map k) a where
  select f = toAscList ^>> unzip ^>> second (streamProcessor f) >>^ uncurry zip >>^ fromAscList


streamProcessor :: ArrowChoice a => a b c -> a [b] [c]
streamProcessor a = proc xs -> case xs of
  b : bs -> do
    (c, cs) <- a *** streamProcessor a -< (b, bs)
    returnA -< c : cs
  _ -> returnA -< []
