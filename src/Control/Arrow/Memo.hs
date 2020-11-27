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
import Data.Hashable
import Data.HashMap.Lazy (HashMap, difference, member, lookup, insert, union)

import Prelude hiding (id, (.), null, lookup)



parallel :: (Arrow a, ArrowSelect a, Traversable f) => Lens' b (f b) -> a b b -> a b b
parallel f = vista f f


vista :: (Arrow a, ArrowSelect a, Traversable f) => Lens' b (f b) -> Lens' c (f c) -> a b c -> a b c
vista descend ascend act = proc b -> do
  (c, cs) <- act *** select (vista descend ascend act) -< (b, b ^. descend)
  returnA -< c & ascend .~ cs


(\\) :: (Eq k, Hashable k) => HashMap k v -> HashMap k v -> HashMap k v
(\\) = difference
infixl 9 \\


data DAG k b = DAG (b -> k) (Lens' b (HashMap k b))

rosetree :: (ArrowSelect a, Traversable f) => Lens' b (f b) -> a b b -> a b b
rosetree descend act = proc b -> do
  models <- select $ rosetree descend act -< b ^. descend
  act -< b & descend .~ models

dag :: (ArrowChoice a, ArrowSelect a, Eq k, Hashable k) => DAG k b -> a b b -> a b b
dag arrows act = id &&& pile arrows ^>> focus arrows act

focus :: (ArrowChoice a, ArrowSelect a, Eq k, Hashable k) => DAG k b -> a b b -> a (b, [HashMap k b]) b
focus arrows act = proc (b, com) -> case com of
  []     -> returnA -< b
  x : xs -> do
    next <- tweak arrows ^<< select act -< x
    focus arrows act -< (next b, fmap next <$> xs)

tweak :: (Eq k, Hashable k) => DAG k b -> HashMap k b -> b -> b
tweak (DAG color _) m b
  | Just c <- color b `lookup` m = c
tweak arrows@(DAG _ descend) m b
  = b & descend %~ fmap (tweak arrows m)

pile :: (Eq k, Hashable k) => DAG k b -> b -> [HashMap k b]
pile arrows b
  = takeWhile (not . null) $ zipWith difference xs (mempty : xs)
  where xs = iterate (leaves arrows b) (leaves arrows b mempty)

leaves :: (Eq k, Hashable k) => DAG k b -> b -> HashMap k b -> HashMap k b
leaves (DAG color _) b m
  | color b `member` m = m
leaves (DAG color descend) b m
  | null $ view descend b \\ m
  = insert (color b) b m
leaves arrows@(DAG _ descend) b m
  = foldr (leaves arrows) m (view descend b)



newtype Memo k v a b c = Memo { memo :: a (b, HashMap k v) (c, HashMap k v) }

memoize :: (Arrow a, Eq k, Hashable k) => Memo k v a (HashMap k v) ()
memoize = Memo $ arr $ \ (b, m) -> ((), union b m)

recall :: Arrow a => Memo k b a () (HashMap k b)
recall = Memo $ arr $ \ (_, s) -> (s, s)


instance (Arrow a, Eq k, Hashable k) => ArrowTransformer (Memo k v) a where
  lift = Memo . first


instance Category a => Category (Memo k v a) where
  id = Memo id
  Memo x . Memo y = Memo (x . y)


instance (Arrow a, Eq k, Hashable k) => Arrow (Memo k v a) where

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


instance (ArrowZero a, Eq k, Hashable k) => ArrowZero (Memo k v a) where
  zeroArrow = Memo zeroArrow

instance (ArrowPlus a, Eq k, Hashable k) => ArrowPlus (Memo k v a) where
  Memo f <+> Memo g = Memo (f <+> g)


instance (ArrowChoice a, Eq k, Hashable k) => ArrowChoice (Memo k v a) where
  f +++ g = Memo $ proc (x, m) -> do
    k <- memo f +++ memo g -< x & (, m) +++ (, m)
    returnA -< (k & fst +++ fst, k & snd ||| snd)


instance (ArrowApply a, Eq k, Hashable k) => ArrowApply (Memo k v a) where
  app = Memo $ arr (\ ((Memo f, x), s) -> (f, (x, s))) >>> app

