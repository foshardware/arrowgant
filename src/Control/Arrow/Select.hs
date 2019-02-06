
module Control.Arrow.Select where

import Control.Arrow

class Arrow a => ArrowSelect a where
  select :: Traversable f => a b c -> a (f b) (f c)
