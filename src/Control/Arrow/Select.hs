{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Control.Arrow.Select where

import Control.Arrow
import Control.Arrow.Transformer
import Control.Lens
import Data.Foldable
import Data.Vector (Vector, fromListN)
import Data.Map (Map, toAscList, fromDistinctAscList)



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

instance ArrowChoice a => ArrowSelect (Map b) a where
  select f = toAscList ^>> unzip ^>> second (select f) >>^ uncurry zip >>^ fromDistinctAscList


streamProcessor :: ArrowChoice a => a b c -> a [b] [c]
streamProcessor a = proc xs -> case xs of
  b : bs -> do
    (c, cs) <- a *** streamProcessor a -< (b, bs)
    returnA -< c : cs
  _ -> returnA -< []

sequential :: ArrowChoice a => a b c -> a [b] [c]
sequential a = proc xs -> case xs of
  b : bs -> do
    c <- a -< b
    cs <- sequential a -< bs
    returnA -< c : cs
  _ -> returnA -< []
