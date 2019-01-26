{-# LANGUAGE Arrows #-}

module Control.Arrow.streamProcessor where

import Control.Arrow
import Control.Applicative
import Data.Foldable


streamProcessor :: ArrowChoice a => a b c -> a [b] [c]
streamProcessor a = proc xs -> case xs of
  b : bs -> do
    (c, cs) <- a *** streamProcessor a -< (b, bs)
    returnA -< c : cs
  _ -> returnA -< []
