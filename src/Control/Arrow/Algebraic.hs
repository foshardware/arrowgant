
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Control.Arrow.Algebraic where

import Control.Arrow
import Control.Arrow.Transformer
import Control.Category
import Control.Monad.Writer (Writer(..), tell, runWriter)
import Prelude hiding (id, (.))

import GHC.Conc (par)


mapReduce :: Arrow a => Algebraic a b c -> Algebraic a b c
mapReduce f = case runWriter $ resolvePar f of
  (g, Found) -> mapReduce g
  (g, _) -> g


data Found = Found | NotFound

instance Semigroup Found where
  Found <> _ = Found
  _ <> Found = Found
  _ <> _ = NotFound

instance Monoid Found where
  mempty = NotFound
  mappend = (<>)


resolvePar :: Arrow a => Algebraic a b c -> Writer Found (Algebraic a b c)

resolvePar Id       = pure id
resolvePar (Lift e) = pure $ lift e
resolvePar (Pure f) = pure $ arr f

-- id . f = f
-- f . id = f
resolvePar (Comp Id f) = resolvePar f
resolvePar (Comp f Id) = resolvePar f

-- arr g . arr f = arr (g . f)
resolvePar (Comp (Pure f) (Pure g)) = resolvePar (Pure (g . f)) <* tell Found

-- arr f *** arr g = arr (\ (x, y) -> (f x, g y))
resolvePar (Split (Pure f) (Pure g)) = resolvePar (Pure (\ (x, y) -> (g y `par` f x, g y))) <* tell Found

-- first f >>> second g = f *** g
-- second g >>> first f = f *** g
resolvePar (Comp (Fst f) (Snd g)) = resolvePar (Split f g) <* tell Found
resolvePar (Comp (Snd g) (Fst f)) = resolvePar (Split f g) <* tell Found

resolvePar (Comp (Fst f) (Comp (Snd g) h)) = resolvePar (Comp (Split f g) h) <* tell Found
resolvePar (Comp (Snd f) (Comp (Fst g) h)) = resolvePar (Comp (Split g f) h) <* tell Found


-- first f >>> first g = first (f >>> g)
-- second f >>> second g = second (f >>> g)
resolvePar (Comp (Fst f) (Fst g)) = resolvePar (Fst (Comp f g)) <* tell Found
resolvePar (Comp (Snd f) (Snd g)) = resolvePar (Snd (Comp f g)) <* tell Found

-- first f >>> g *** h = (f >>> g) *** h
-- second f >>> g *** h = g *** (f >>> h)
resolvePar (Comp (Fst f) (Split g h)) = resolvePar (Split (Comp f g) h) <* tell Found
resolvePar (Comp (Snd f) (Split g h)) = resolvePar (Split g (Comp f h)) <* tell Found

resolvePar (Comp (Fst f) (Comp (Split g h) i)) = resolvePar (Comp (Split (Comp f g) h) i) <* tell Found
resolvePar (Comp (Snd f) (Comp (Split g h) i)) = resolvePar (Comp (Split g (Comp f h)) i) <* tell Found


-- f *** g >>> first h = (f >>> h) *** g
-- f *** g >>> second h = f *** (g >>> h)
resolvePar (Comp (Split f g) (Fst h)) = resolvePar (Split (Comp f h) g) <* tell Found
resolvePar (Comp (Split f g) (Snd h)) = resolvePar (Split f (Comp g h)) <* tell Found

-- id *** f = second f
-- f *** id = first f
resolvePar (Split Id f) = resolvePar (Snd f) <* tell Found
resolvePar (Split f Id) = resolvePar (Fst f) <* tell Found


resolvePar (Comp f g) = Comp <$> resolvePar f <*> resolvePar g

resolvePar (Fst f) = Fst <$> resolvePar f
resolvePar (Snd f) = Snd <$> resolvePar f

resolvePar (Split f g) = Split <$> resolvePar f <*> resolvePar g



data Algebraic a b c where

  Id     :: Algebraic a b b
  Lift   :: a b c -> Algebraic a b c
  Pure   :: (b -> c) -> Algebraic a b c

  Comp   :: Algebraic a b c -> Algebraic a c d -> Algebraic a b d
  Split  :: Algebraic a b c -> Algebraic a d e -> Algebraic a (b, d) (c, e)

  Fst    :: Algebraic a b c -> Algebraic a (b, d) (c, d)
  Snd    :: Algebraic a b c -> Algebraic a (d, b) (d, c)


instance Arrow a => ArrowTransformer Algebraic a where
  lift = Lift


algebraic :: Arrow a => Algebraic a b c -> a b c

algebraic Id          = id
algebraic (Lift e)    = e
algebraic (Pure f)    = arr f

algebraic (Comp f g)  = algebraic g . algebraic f

algebraic (Fst f)  = first  (algebraic f)
algebraic (Snd f)  = second (algebraic f)

algebraic (Split f g) = algebraic f *** algebraic g


instance Category (Algebraic a) where
  id = Id
  g . f = Comp f g

instance Arrow (Algebraic a) where
  arr    = Pure
  first  = Fst
  second = Snd
  (***) = Split

instance ArrowZero a => ArrowZero (Algebraic a) where
  zeroArrow = lift zeroArrow

instance ArrowPlus a => ArrowPlus (Algebraic a) where
  f <+> g = lift (algebraic f <+> algebraic g)

instance ArrowChoice a => ArrowChoice (Algebraic a) where
  f +++ g = lift (algebraic f +++ algebraic g)

