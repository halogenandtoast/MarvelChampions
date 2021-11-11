module Marvel.Boost where

import Marvel.Prelude

data BoostIcon = Boost | Star
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

boostCount :: [BoostIcon] -> Natural
boostCount = foldr ((+) . boostValue) 0
 where
  boostValue Boost = 1
  boostValue Star = 0
