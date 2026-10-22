module Marvel.Cost.Types where

import Marvel.Prelude

import Data.List (nub)
import Marvel.Ref
import Marvel.Resource.Types

data Cost
  = Costs [Cost]
  | ResourceCost (Maybe Resource)
  | DynamicResourceCost (Maybe Resource)
  | MultiResourceCost [Maybe Resource]
  | DamageCost Natural
  | HealCost Natural
  | DamageThisCost Natural
  | ExhaustCost
  | DiscardHandCardCost Natural
  | DiscardCost Target
  | UseCost
  | NoCost
  | -- | as many resources, each of a different type
    DifferentResourcesCost Natural
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
costResources (DiscardCost _) = []
costResources (DiscardHandCardCost _) = []
costResources (DamageCost _) = []
costResources (HealCost _) = []
costResources (DamageThisCost _) = []
costResources UseCost = []
costResources (DynamicResourceCost _) = []
costResources (ResourceCost r) = [r]
costResources (MultiResourceCost rs) = rs
costResources (Costs ps) = concatMap costResources ps
costResources (DifferentResourcesCost n) = replicate (fromIntegral n) Nothing

-- | How many resources of different types the cost asks for.
differentResourcesNeeded :: Cost -> Natural
differentResourcesNeeded = \case
  DifferentResourcesCost n -> n
  Costs cs -> sum (map differentResourcesNeeded cs)
  _ -> 0

-- | Whether the resources can be told as that many of different types: a wild one may
-- be any type, wild among them.
coversDifferentTypes :: Natural -> [Resource] -> Bool
coversDifferentTypes n rs =
  fromIntegral (min 4 (length (nub (filter (/= Wild) rs)) + length (filter (== Wild) rs))) >= n
