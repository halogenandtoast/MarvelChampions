module Marvel.Cost where

import Marvel.Prelude

import Marvel.Resource

data Cost
  = Costs [Cost]
  | ResourceCost (Maybe Resource)
  | MultiResourceCost [Maybe Resource]
  | DamageCost Natural
  | HealCost Natural
  | DamageThisCost Natural
  | ExhaustCost
  | DiscardHandCardCost Natural
  | UseCost
  | NoCost
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
costResources (DiscardHandCardCost _) = []
costResources (DamageCost _) = []
costResources (HealCost _) = []
costResources (DamageThisCost _) = []
costResources UseCost = []
costResources (ResourceCost r) = [r]
costResources (MultiResourceCost rs) = rs
costResources (Costs ps) = concatMap costResources ps
