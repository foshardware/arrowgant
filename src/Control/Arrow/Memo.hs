{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ParallelListComp #-}

module Control.Arrow.Memo where

import Control.Arrow
import Control.Arrow.Transformer
import Control.Category
import Control.Lens

import Data.Foldable
import Data.Map (Map, difference, member, lookup, insert, union)

import Prelude hiding (id, (.), null, lookup)


leaves :: (Foldable f, Ord k) => (b -> k) -> Lens' b (f b) -> b -> Map k b -> Map k b
leaves color       _ b m
  | color b `member` m = m
leaves color descend b m
  | b ^. descend & all (\ c -> color c `member` m)
  = insert (color b) b m
leaves color descend b m
  = foldr (leaves color descend) m (b ^. descend)


layers :: (Foldable f, Ord k) => (b -> k) -> Lens' b (f b) -> b -> [Map k b]
layers color descend b
  = takeWhile (not . null) $ zipWith difference xs (mempty : xs)
  where xs = iterate (leaves color descend b) (leaves color descend b mempty)



newtype Memo k v a b c = Memo { memo :: a (b, Map k v) (c, Map k v) }

memoize :: (Arrow a, Ord k) => Memo k v a (Map k v) ()
memoize = Memo $ arr $ \ (b, m) -> ((), union b m)

recall :: Arrow a => Memo k b a () (Map k b)
recall = Memo $ arr $ \ (_, s) -> (s, s)


reflect :: (ArrowChoice a, Ord k) => (b -> k) -> Memo k c a b c -> Memo k c a b c
reflect color act = proc b -> do
  m <- recall -< ()
  case color b `lookup` m of
    Just c -> returnA -< c
    Nothing -> act -< b


instance Arrow a => ArrowTransformer (Memo k v) a where
  lift = Memo . first


instance Category a => Category (Memo k v a) where
  id = Memo id
  Memo x . Memo y = Memo (x . y)


instance Arrow a => Arrow (Memo k v a) where
  arr = Memo . arr . first
  first = Memo . (>>>) swapSnd . (<<<) swapSnd . first . memo


swapSnd :: Arrow r => r ((a, b), c) ((a, c), b)
swapSnd = arr $ \ ((x, y), z) -> ((x, z), y)


instance ArrowChoice a => ArrowChoice (Memo k v a) where
  a +++ b = Memo $ proc (s, m) -> case s of
    Left x -> first (arr Left) <<< memo a -< (x, m)
    Right x -> first (arr Right) <<< memo b -< (x, m)


instance ArrowApply a => ArrowApply (Memo k v a) where
  app = Memo $ arr (\ ((Memo f, x), s) -> (f, (x, s))) >>> app

