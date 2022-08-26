module Marvel.Upgrade.Upgrades.EnergyChannel
  ( energyChannel
  , EnergyChannel(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Resource
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

energyChannel :: UpgradeCard EnergyChannel
energyChannel = upgrade EnergyChannel Cards.energyChannel

newtype EnergyChannel = EnergyChannel (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance HasAbilities EnergyChannel where
  getAbilities a =
    [ ability a 1 Action OwnsThis (DynamicResourceCost $ Just Energy)
      $ RunAbility (toTarget a) 1
    , subtype Ability.Attack
      $ ability a 2 HeroAction OwnsThis (DiscardCost $ toTarget a)
      $ RunAbility (toTarget a) 2
    ]

instance RunMessage EnergyChannel where
  runMessage msg u@(EnergyChannel attrs) = case msg of
    RanAbility (isTarget attrs -> True) 1 _ -> pure u
    _ -> EnergyChannel <$> runMessage msg attrs
