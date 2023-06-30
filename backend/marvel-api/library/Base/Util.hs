module Base.Util where

import Prelude

without :: Int -> [a] -> [a]
without n as = [a | (i, a) <- zip [0 ..] as, i /= n]
