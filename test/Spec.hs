
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
sym = lift . symbol

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
