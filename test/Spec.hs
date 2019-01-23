{-# LANGUAGE GADTs #-}

import Control.Category
import Control.Arrow
import Control.Arrow.Transformer
import Prelude hiding (id, (.))

import Test.Tasty
import Test.Tasty.HUnit

import Control.Arrow.Algebraic

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "id . f = f" $ id . f ==> f
  , testCase "f . id = f" $ f . id ==> f

  , testCase "id *** f = second f"
    $ id *** f ==> second f
  , testCase "f *** id = first f"
    $ f *** id ==> first f

  , testCase "first f >>> second g = f *** g"
    $ first f >>> second g ==> f *** g
  , testCase "second f >>> first g = g *** f"
    $ second f >>> first g ==> g *** f

  , testCase "first f >>> first g = first (f >>> g)"
    $ first f >>> first g ==> first (f >>> g)
  , testCase "second f >>> second g = second (f >>> g)"
    $ second f >>> second g ==> second (f >>> g)

  , testCase "first f >>> g *** h = (f >>> g) *** h"
    $ first f >>> g *** h ==> (f >>> g) *** h
  , testCase "second f >>> g *** h = g *** (f >>> h)"
    $ second f >>> g *** h ==> g *** (f >>> h)

  , testCase "f *** g >>> first h = (f >>> h) *** g"
    $ f *** g >>> first h ==> (f >>> h) *** g
  , testCase "f *** g >>> second h = f *** (g >>> h)"
    $ f *** g >>> second h ==> f *** (g >>> h)

  , testCase "first f >>> first g >>> first h = first (f >>> g >>> h)"
    $ first f >>> first g >>> first h ==> first (f >>> g >>> h)
  , testCase "second f >>> second g >>> second h = second (f >>> g >>> h)"
    $ second f >>> second g >>> second h ==> second (f >>> g >>> h)

  ]


infixr 0 ==>

(==>) :: Algebraic Computation a b -> Algebraic Computation a b -> IO ()
f ==> g = algebraic (reducer f) @?= algebraic g

reducer :: Algebraic Computation a b -> Algebraic Computation a b
reducer = mapReduce


sym :: Int -> Algebraic Computation a b
sym = lift . Symbol

a, b, c, d, e, f, g :: Algebraic Computation a b
a = sym 1
b = sym 2
c = sym 3
d = sym 4
e = sym 5
f = sym 6
g = sym 7
h = sym 8
i = sym 9


data Computation a b where

  Symbol :: Int -> Computation a b
  PropId :: Computation a b
  Append :: Computation b c -> Computation a b -> Computation a c

  SomeFunc :: Computation a b

  First  :: Computation a b -> Computation a b
  Second :: Computation a b -> Computation a b

  Par    :: Computation a b -> Computation c d -> Computation e f
  Choice :: Computation a b -> Computation c d -> Computation e f

  Zero :: Computation a b
  Plus :: Computation a b -> Computation a b -> Computation a b


cast :: Computation a b -> Computation c d
cast (Symbol x) = Symbol x
cast PropId = PropId
cast SomeFunc = SomeFunc
cast (Append g f) = Append (cast g) (cast f)
cast (First  f) = First  (cast f)
cast (Second f) = Second (cast f)
cast (Par f g) = Par (cast f) (cast g)
cast Zero = Zero
cast (Plus f g) = Plus (cast f) (cast g)
cast (Choice f g) = Choice (cast f) (cast g)



instance Category Computation where
  id = PropId
  (.) = Append

instance Arrow Computation where
  arr f = SomeFunc
  first  f = First  (cast f)
  second f = Second (cast f)
  f *** g = Par f g

instance ArrowZero Computation where
  zeroArrow = Zero

instance ArrowPlus Computation where
  (<+>) = Plus

instance ArrowChoice Computation where
  (+++) = Choice


instance Eq (Computation a b) where

  Symbol x == Symbol y = x == y
  PropId == PropId = True
  Append g f == Append i h = g == cast i && f == cast h

  SomeFunc == SomeFunc = True
  First  f == First  g = f == g
  Second f == Second g = f == g

  Par f g == Par h i = f == cast h && g == cast i

  Zero == Zero = True
  Plus f g == Plus h i = f == h && g == i

  Choice f g == Choice h i = f == cast h && g == cast i

  _ == _ = False


instance Show (Computation a b) where
  show (Symbol x) = show x
  show PropId = "id"
  show (Append f g) = unwords [paren g, ">>>", paren f]
  show (First  f) = unwords ["first", paren f]
  show (Second f) = unwords ["second", paren f]
  show (Par f g) = unwords [paren f, "***", paren g]
  show Zero = "zeroArrow"
  show (Plus f g) = unwords [paren f, "<+>", paren g]
  show (Choice f g) = unwords [paren f, "+++", paren g]

paren :: Computation a b -> String
paren (Symbol 1) = "a"
paren (Symbol 2) = "b"
paren (Symbol 3) = "c"
paren (Symbol 4) = "d"
paren (Symbol 5) = "e"
paren (Symbol 6) = "f"
paren (Symbol 7) = "g"
paren (Symbol 8) = "h"
paren (Symbol 9) = "i"
paren (Symbol x) = show x
paren x = "(" ++ show x ++ ")"

