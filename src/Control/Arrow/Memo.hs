{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

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
    Just  c -> returnA -< c
    Nothing -> act -< b


instance (Arrow a, Ord k) => ArrowTransformer (Memo k v) a where
  lift = Memo . first


instance Category a => Category (Memo k v a) where
  id = Memo id
  Memo x . Memo y = Memo (x . y)


instance (Arrow a, Ord k) => Arrow (Memo k v a) where

  arr = Memo . arr . first

  first f = Memo $ proc ((x, y), m) -> do
    ((x', m'), _) <- first (memo f) -< ((x, m), y)
    returnA -< ((x', y), m')

  second f = Memo $ proc ((x, y), m) -> do
    (_, (y', m')) <- second (memo f) -< (x, (y, m))
    returnA -< ((x, y'), m')

  f *** g = Memo $ proc ((x, y), m) -> do
    ((x', m'), (y', m'')) <- memo f *** memo g -< ((x, m), (y, m))
    returnA -< ((x', y'), union m' m'')


instance (ArrowZero a, Ord k) => ArrowZero (Memo k v a) where
  zeroArrow = Memo zeroArrow

instance (ArrowPlus a, Ord k) => ArrowPlus (Memo k v a) where
  Memo f <+> Memo g = Memo (f <+> g)


instance (ArrowChoice a, Ord k) => ArrowChoice (Memo k v a) where
  f +++ g = Memo $ proc (x, m) -> do
    k <- memo f +++ memo g -< x & (, m) +++ (, m)
    returnA -< (k & fst +++ fst, k & snd ||| snd)


instance (ArrowApply a, Ord k) => ArrowApply (Memo k v a) where
  app = Memo $ arr (\ ((Memo f, x), s) -> (f, (x, s))) >>> app

