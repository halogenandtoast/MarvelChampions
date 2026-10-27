module Marvel.Cost.Types where

import Marvel.Prelude

import Data.List (nub)
import {-# SOURCE #-} Marvel.Matchers.Types (UpgradeMatcher)
import Marvel.Ref
import Marvel.Resource.Types
import Marvel.Trait.Types (Trait)

data Cost
  = Costs [Cost]
  | ResourceCost (Maybe Resource)
  | DynamicResourceCost (Maybe Resource)
  | MultiResourceCost [Maybe Resource]
  | DamageCost Natural
  | HealCost Natural
  | DamageThisCost Natural
  | ExhaustCost
  | -- | at least so many cards from hand, and at most so many (any number for Nothing)
    DiscardHandCardsCost Natural (Maybe Natural)
  | DiscardCost Target
  | -- | so many cards discarded at random from the payer's hand
    DiscardRandomHandCardsCost Natural
  | UseCost
  | NoCost
  | -- | as many resources, each of a different type
    DifferentResourcesCost Natural
  | -- | exhaust an upgrade the payer controls, chosen among those matching
    ExhaustUpgradeCost UpgradeMatcher
  | -- | return an upgrade the payer controls, chosen among those matching, to hand
    ReturnUpgradeToHandCost UpgradeMatcher
  | -- | discard an upgrade the payer controls, chosen among those matching
    DiscardUpgradeCost UpgradeMatcher
  | -- | discard the top card of the payer's deck: not payable with an empty deck
    DiscardTopOfDeckCost
  | -- | exhaust at least so many allies the payer controls, and at most so many (any
    -- number for Nothing)
    ExhaustAlliesCost Natural (Maybe Natural)
  | -- | exhaust a character the payer controls with the trait: their identity or an ally
    ExhaustCharacterWithTraitCost Trait
  | -- | discard encounter cards until a minion is discarded, and put it into play
    -- engaged with the payer
    DiscardEncounterCardsUntilMinionCost
  | -- | "deal yourself 1 facedown encounter card"
    DealEncounterCardCost
  | -- | the printed resource cost of the card chosen as the card's target
    TargetPrintedCost
  | -- | nothing more is due, but the payer may go on spending resources: "for each
    -- resource you overpaid" (Ant-Man, Hank Pym)
    MayOverpay
  | -- | so many counters removed from the payer's identity card: "remove 1 growth
    -- counter from him" (Groot)
    IdentityCountersCost Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

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
costResources (DiscardHandCardsCost _ _) = []
costResources (DiscardRandomHandCardsCost _) = []
costResources (ExhaustUpgradeCost _) = []
costResources (ReturnUpgradeToHandCost _) = []
costResources (DiscardUpgradeCost _) = []
costResources DiscardTopOfDeckCost = []
costResources (ExhaustAlliesCost _ _) = []
costResources (ExhaustCharacterWithTraitCost _) = []
costResources DiscardEncounterCardsUntilMinionCost = []
costResources DealEncounterCardCost = []
costResources TargetPrintedCost = []
costResources MayOverpay = []
costResources (IdentityCountersCost _) = []
costResources (DamageCost _) = []
costResources (HealCost _) = []
costResources (DamageThisCost _) = []
costResources UseCost = []
-- | X is at least 1
costResources (DynamicResourceCost r) = [r]
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
