module Marvel.Cost where

import Marvel.Prelude

import Marvel.Resource

data Cost = Costs [Cost] | ResourceCost (Maybe Resource) | ExhaustCost | NoCost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Monoid Cost where
  mempty = NoCost

instance Semigroup Cost where
  NoCost <> x = x
  x <> NoCost = x
  Costs xs <> Costs ys = Costs $ xs <> ys
  Costs xs <> y = Costs $ xs <> [y]
  x <> Costs ys = Costs $ x : ys
  x <> y = Costs [x, y]

costResources :: Cost -> [Maybe Resource]
costResources NoCost = []
costResources ExhaustCost = []
costResources (ResourceCost r) = [r]
costResources (Costs ps) = concatMap costResources ps
