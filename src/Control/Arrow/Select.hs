
module Control.Arrow.Select where

import Control.Arrow
import Control.Category
import Prelude hiding (id)


class Arrow a => ArrowSelect a where

  select :: Traversable f => a b c -> a (f b) (f c)


infixr 4 ///, \\\

class ArrowChoice a => ArrowRace a where

  (///) :: a b c -> a b d -> a b (Either c d)

  (\\\) :: a b c -> a b c -> a b c
  a \\\ b = a /// b >>> id ||| id
