-- | Keep all relevant cases, then sample distinct additional cases reproducibly.
module SmokeSelection (selectSmokeCases) where

import Prelude
import Data.List (nub, partition)
import System.Random (mkStdGen, randomR)

selectSmokeCases :: Eq a => Int -> Int -> (a -> Bool) -> [a] -> [a]
selectSmokeCases seed extra relevant candidates =
  filter (`elem` selected) unique
 where
  unique = nub candidates
  (focused, others) = partition relevant unique
  selected = focused <> sample (mkStdGen seed) (max 0 extra) others
  sample _ 0 _ = []
  sample _ _ [] = []
  sample gen n xs =
    let (i, next) = randomR (0, length xs - 1) gen
        (before, rest) = splitAt i xs
     in case rest of
          [] -> []
          chosen : after -> chosen : sample next (n - 1) (before <> after)
