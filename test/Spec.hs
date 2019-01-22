
import Control.Category
import Control.Arrow
import Control.Arrow.Transformer
import Prelude hiding (id, (.))

import Lib

main :: IO ()
main = pure ()

sym :: Int -> Algebraic Computation a a
sym = lift . symbol

test :: Algebraic Computation a a
test = mapReduce $ id >>> sym 1


-- first a >>> second b = a *** b
