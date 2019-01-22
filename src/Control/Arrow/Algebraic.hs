
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

mapReduce Zero = Zero
mapReduce (Plus f g) = Plus (mapReduce f) (mapReduce g)

mapReduce (Choice f g) = Choice (mapReduce f) (mapReduce g)


data Algebraic a b c where

  Id     :: Algebraic a b b
  Lift   :: a b c -> Algebraic a b c
  Pure   :: (b -> c) -> Algebraic a b c

  Comp   :: Algebraic a b c -> Algebraic a c d -> Algebraic a b d
  Split  :: Algebraic a b c -> Algebraic a d e -> Algebraic a (b, d) (c, e)

  Fst    :: Algebraic a b c -> Algebraic a (b, d) (c, d)
  Snd    :: Algebraic a b c -> Algebraic a (d, b) (d, c)

  Zero   :: Algebraic a b c
  Plus   :: Algebraic a b c -> Algebraic a b c -> Algebraic a b c

  Choice :: Algebraic a b c -> Algebraic a d e -> Algebraic a (Either b d) (Either c e)


instance Arrow a => ArrowTransformer Algebraic a where
  lift = Lift


algebraic :: (ArrowChoice a, ArrowPlus a) => Algebraic a b c -> a b c

algebraic Id          = id
algebraic (Lift e)    = e
algebraic (Pure f)    = arr f

algebraic (Comp f g)  = algebraic g . algebraic f

algebraic (Fst f)  = first  (algebraic f)
algebraic (Snd f)  = second (algebraic f)

algebraic (Split f g) = algebraic f *** algebraic g

algebraic Zero       = zeroArrow
algebraic (Plus f g) = algebraic f <+> algebraic g

algebraic (Choice f g)   = algebraic f +++ algebraic g


instance Category (Algebraic a) where
  id = Id
  g . f = Comp f g

instance Arrow (Algebraic a) where
  arr    = Pure
  first  = Fst
  second = Snd
  (***)  = Split

instance ArrowZero (Algebraic a) where
  zeroArrow = Zero

instance ArrowPlus (Algebraic a) where
  (<+>) = Plus

instance ArrowChoice (Algebraic a) where
  (+++) = Choice


data Computation a b where

  Symbol :: Int -> Computation a b
  PropId :: Computation a b
  Append :: Computation b c -> Computation a b -> Computation a c

  SomeFunc :: Computation a b

  First  :: Computation a b -> Computation a b
  Second :: Computation a b -> Computation a b

  PropSplit  :: Computation a b -> Computation c d -> Computation e f
  PropChoice :: Computation a b -> Computation c d -> Computation e f

  PropZero :: Computation a b
  PropPlus :: Computation a b -> Computation a b -> Computation a b



symbol :: Int -> Computation a b
symbol = Symbol


cast :: Computation a b -> Computation c d
cast (Symbol x) = Symbol x
cast PropId = PropId
cast SomeFunc = SomeFunc
cast (Append g f) = Append (cast g) (cast f)
cast (First  f) = First  (cast f)
cast (Second f) = Second (cast f)
cast (PropSplit f g) = PropSplit (cast f) (cast g)
cast PropZero = PropZero
cast (PropPlus f g) = PropPlus (cast f) (cast g)
cast (PropChoice f g) = PropChoice (cast f) (cast g)



instance Category Computation where
  id = PropId
  (.) = Append

instance Arrow Computation where
  arr f = SomeFunc
  first  f = First  (cast f)
  second f = Second (cast f)

  f *** g = PropSplit f g

instance ArrowZero Computation where
  zeroArrow = PropZero

instance ArrowPlus Computation where
  (<+>) = PropPlus

instance ArrowChoice Computation where
  (+++) = PropChoice


instance Eq (Computation a b) where

  Symbol x == Symbol y = x == y
  PropId == PropId = True
  Append g f == Append i h = g == cast i && f == cast h

  SomeFunc == SomeFunc = True
  First  f == First  g = f == g
  Second f == Second g = f == g

  PropSplit f g == PropSplit h i = f == cast h && g == cast i

  PropZero == PropZero = True
  PropPlus f g == PropPlus h i = f == h && g == i

  PropChoice f g == PropChoice h i = f == cast h && g == cast i

  _ == _ = False

