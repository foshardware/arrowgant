{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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



parallel :: (Arrow a, ArrowSelect f a) => Lens' b (f b) -> a b b -> a b b
parallel f = vista f f


vista :: (Arrow a, ArrowSelect f a) => Lens' b (f b) -> Lens' c (f c) -> a b c -> a b c
vista descend ascend act = proc b -> do
  (c, cs) <- act *** select (vista descend ascend act) -< (b, b ^. descend)
  returnA -< c & ascend .~ cs


dag
  :: (ArrowChoice a, ArrowSelect f (Memo k b a), Foldable f, Ord k)
  => (b -> k) -> Lens' b (f b) -> a b b
  -> a b b
dag color descend act = arr (, mempty) >>> memo (focus color descend act) >>> arr fst

focus
  :: (ArrowChoice a, ArrowSelect f (Memo k b a), Foldable f, Ord k)
  => (b -> k) -> Lens' b (f b) -> a b b
  -> Memo k b a b b
focus color descend act = proc b -> do
  tone color descend act -< b
  hierarchical descend $ reflect color $ lift act -< b


tone
  :: (ArrowChoice a, Foldable f, Ord k)
  => (b -> k) -> Lens' b (f b) -> a b c
  -> Memo k c a b ()
tone color descend act = proc b -> do
  sequential $ memoize <<< lift (select act) -< layers color descend b
  returnA -< ()


hierarchical :: (Arrow a, ArrowSelect f a) => Lens' b (f b) -> a b b -> a b b
hierarchical descend act = proc b -> do
  models <- select $ hierarchical descend act -< b ^. descend
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
