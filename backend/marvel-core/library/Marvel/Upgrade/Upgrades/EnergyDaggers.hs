module Marvel.Upgrade.Upgrades.EnergyDaggers
  ( energyDaggers
  , EnergyDaggers(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Cards qualified as Cards

energyDaggers :: UpgradeCard EnergyDaggers
energyDaggers = upgrade EnergyDaggers Cards.energyDaggers

newtype EnergyDaggers = EnergyDaggers UpgradeAttrs
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities EnergyDaggers where
  getAbilities _ = []

instance RunMessage EnergyDaggers where
  runMessage msg (EnergyDaggers attrs) = EnergyDaggers <$> runMessage msg attrs
