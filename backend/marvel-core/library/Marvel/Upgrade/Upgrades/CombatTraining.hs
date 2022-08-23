module Marvel.Upgrade.Upgrades.CombatTraining
  ( combatTraining
  , CombatTraining(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Types
import Marvel.Upgrade.Cards qualified as Cards

combatTraining :: UpgradeCard CombatTraining
combatTraining = upgrade CombatTraining Cards.combatTraining

newtype CombatTraining = CombatTraining UpgradeAttrs
  deriving anyclass IsUpgrade
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor CombatTraining where
  getModifiersFor _ (IdentityTarget iid) (CombatTraining a)
    | iid == upgradeController a = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities CombatTraining where
  getAbilities _ = []

instance RunMessage CombatTraining where
  runMessage msg (CombatTraining attrs) =
    CombatTraining <$> runMessage msg attrs
