{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Control.Arrow.Memo where

import Control.Arrow
import Control.Arrow.Select
import Control.Arrow.Transformer
import Control.Category
import Control.Lens

import Data.Foldable
import Data.Map (Map, difference, member, lookup, insert, union)

import Prelude hiding (id, (.), null, lookup)



parallel :: (Arrow a, ArrowSelect f a) => Lens' b (f b) -> a b b -> a b b
parallel f = vista f f


vista :: (Arrow a, ArrowSelect f a) => Lens' b (f b) -> Lens' c (f c) -> a b c -> a b c
vista descend ascend act = proc b -> do
  (c, cs) <- act *** select (vista descend ascend act) -< (b, b ^. descend)
  returnA -< c & ascend .~ cs


data DAG k b = DAG (b -> k) (Lens' b (Map k b))

rosetree :: (ArrowChoice a, ArrowSelect f a) => Lens' b (f b) -> a b b -> a b b
rosetree descend act = proc b -> do
  models <- select $ rosetree descend act -< b ^. descend
  act -< b & descend .~ models

dag :: (ArrowChoice a, Ord k) => DAG k b -> a b b -> a b b
dag arrows act = proc b -> do
  (c, _) <- memo $ focus arrows act -< ((b, layers arrows b), mempty)
  returnA -< c

focus :: (ArrowChoice a, Ord k) => DAG k b -> a b b -> Memo k b a (b, [Map k b]) b
focus arrows act = proc (b, com) -> case com of
  []     -> returnA -< b
  x : xs -> do
    next <- adjust arrows ^<< memoize <<< lift (select act) -< x
    focus arrows act -< (next b, fmap next <$> xs)

adjust :: Ord k => DAG k b -> Map k b -> b -> b
adjust (DAG color _) m b
  | Just c <- color b `lookup` m = c
adjust arrows@(DAG _ descend) m b
  = b & descend %~ fmap (adjust arrows m)

leaves :: Ord k => DAG k b -> b -> Map k b -> Map k b
leaves (DAG color _) b m
  | color b `member` m = m
leaves (DAG color descend) b m
  | b ^. descend & all (\ c -> color c `member` m)
  = insert (color b) b m
leaves arrows@(DAG _ descend) b m
  = foldr (leaves arrows) m (b ^. descend)

layers :: Ord k => DAG k b -> b -> [Map k b]
layers arrows b
  = takeWhile (not . null) $ zipWith difference xs (mempty : xs)
  where xs = iterate (leaves arrows b) (leaves arrows b mempty)


newtype Memo k v a b c = Memo { memo :: a (b, Map k v) (c, Map k v) }

memoize :: (Arrow a, Ord k) => Memo k v a (Map k v) (Map k v)
memoize = Memo $ arr $ \ (b, m) -> (b, union b m)

recall :: Arrow a => Memo k b a () (Map k b)
recall = Memo $ arr $ \ (_, s) -> (s, s)


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

