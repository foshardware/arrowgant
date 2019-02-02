{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ParallelListComp #-}

module Control.Arrow.Select where

import Control.Arrow
import Control.Arrow.Memo
import Control.Arrow.Transformer
import Control.Lens
import Data.Foldable
import Data.Vector (Vector, fromListN)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set



dag :: (ArrowChoice a, Foldable f, Ord k) => (b -> k) -> Lens' b (f b) -> a b c -> a b c
dag color descend act = proc b -> do
  (c, _) <- memo $ focus color descend $ lift act -< (b, mempty)
  returnA -< c

focus :: (ArrowChoice a, Foldable f, Ord k) => (b -> k) -> Lens' b (f b) -> Memo k c a b c -> Memo k c a b c
focus color descend act = proc b -> do
  sequential $ memoize <<< select act -< layers color descend b
  reflect color act -< b


hierarchical :: (Arrow a, ArrowSelect f a) => Lens' b (f b) -> a b b -> a b b
hierarchical descend act = proc b -> do
  models <- hierarchical descend act & select -< b ^. descend
  act -< b & descend .~ models


parallel :: (Arrow a, ArrowSelect f a) => Lens' b (f b) -> a b b -> a b b
parallel f = vista f f


vista :: (Arrow a, ArrowSelect f a) => Lens' b (f b) -> Lens' c (f c) -> a b c -> a b c
vista descend ascend act = proc b -> do
  (c, cs) <- act *** (vista descend ascend act & select) -< (b, b ^. descend)
  returnA -< c & ascend .~ cs



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

instance ArrowChoice a => ArrowSelect Set a where
  select f = Set.toAscList ^>> select f >>^ Set.fromDistinctAscList

instance ArrowChoice a => ArrowSelect (Map b) a where
  select f = Map.toAscList ^>> unzip ^>> second (select f) >>^ uncurry zip >>^ Map.fromDistinctAscList



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