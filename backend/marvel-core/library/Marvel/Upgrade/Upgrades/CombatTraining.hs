module Marvel.Upgrade.Upgrades.CombatTraining (
  combatTraining,
  CombatTraining (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

combatTraining :: UpgradeCard CombatTraining
combatTraining = upgrade CombatTraining Cards.combatTraining

newtype CombatTraining = CombatTraining (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasModifiersFor CombatTraining where
  getModifiersFor _ (IdentityRef iid) (CombatTraining a)
    | iid == upgradeController a = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities CombatTraining where
  getAbilities _ = []

instance RunMessage CombatTraining where
  runMessage msg (CombatTraining attrs) =
    CombatTraining <$> runMessage msg attrs
