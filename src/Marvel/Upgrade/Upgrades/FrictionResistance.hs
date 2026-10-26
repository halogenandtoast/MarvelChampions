module Marvel.Upgrade.Upgrades.FrictionResistance (
  frictionResistance,
  FrictionResistance (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

frictionResistance :: UpgradeCard FrictionResistance
frictionResistance = upgrade FrictionResistance Cards.frictionResistance

newtype FrictionResistance = FrictionResistance (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Hero Response: After you ready Quicksilver, ready this card." Any time his
-- exhausted hero turns ready, by a card or the ready step; offered only while this card
-- is exhausted, as readying it is all it does. "Resource: Exhaust Friction Resistance →
-- generate a [physical] resource."
instance HasAbilities FrictionResistance where
  getAbilities (FrictionResistance a) =
    [ limitedWindowAbility a 1 (IdentityReadies You) HeroResponse OwnsThis NoCost (runAbility a 1)
    | upgradeExhausted a
    ]
      <> [ability a 2 Resource OwnsThis ExhaustCost (Pay $ ResourcePayment Physical)]

instance RunMessage FrictionResistance where
  runMessage msg u@(FrictionResistance attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      ready (upgradeId attrs)
      pure u
    _ -> FrictionResistance <$> runMessage msg attrs
