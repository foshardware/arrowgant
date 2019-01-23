
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Control.Arrow.Algebraic where

import Control.Arrow
import Control.Arrow.Transformer
import Control.Category
import Prelude hiding (id, (.))


mapReduce :: Arrow a => Algebraic a b c -> Algebraic a b c

mapReduce Id       = id
mapReduce (Lift e) = Lift e
mapReduce (Pure f) = arr f

-- id . f = f
-- f . id = f
mapReduce (Comp Id f) = mapReduce f
mapReduce (Comp f Id) = mapReduce f

-- arr g . arr f = arr (g . f)
mapReduce (Comp (Pure f) (Pure g)) = mapReduce (Pure (g . f))

-- first f >>> second g = f *** g
-- second g >>> first f = f *** g
mapReduce (Comp (Fst f) (Snd g)) = mapReduce (Split f g)
mapReduce (Comp (Snd g) (Fst f)) = mapReduce (Split f g)

-- first f >>> first g = first (f >>> g)
-- second f >>> second g = second (f >>> g)
mapReduce (Comp (Fst f) (Fst g)) = mapReduce (Fst (Comp f g))
mapReduce (Comp (Snd f) (Snd g)) = mapReduce (Snd (Comp f g))

-- first f >>> g *** h = (f >>> g) *** h
-- second f >>> g *** h = g *** (f >>> h)
mapReduce (Comp (Fst f) (Split g h)) = mapReduce (Split (Comp f g) h)
mapReduce (Comp (Snd f) (Split g h)) = mapReduce (Split g (Comp f h))

-- f *** g >>> first h = (f >>> h) *** g
-- f *** g >>> second h = f *** (g >>> h)
mapReduce (Comp (Split f g) (Fst h)) = mapReduce (Split (Comp f h) g)
mapReduce (Comp (Split f g) (Snd h)) = mapReduce (Split f (Comp g h))

-- id *** f = second f
-- f *** id = first f
mapReduce (Split Id f) = mapReduce (Snd f)
mapReduce (Split f Id) = mapReduce (Fst f)


mapReduce (Comp f g) = Comp (mapReduce f) (mapReduce g)

mapReduce (Fst f) = Fst (mapReduce f)
mapReduce (Snd f) = Snd (mapReduce f)

mapReduce (Split f g) = Split (mapReduce f) (mapReduce g)



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
  (***)  = Split

instance ArrowZero a => ArrowZero (Algebraic a) where
  zeroArrow = lift zeroArrow

instance ArrowPlus a => ArrowPlus (Algebraic a) where
  f <+> g = lift (algebraic f <+> algebraic g)

instance ArrowChoice a => ArrowChoice (Algebraic a) where
  f +++ g = lift (algebraic f +++ algebraic g)

