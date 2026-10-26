module Marvel.Upgrade.Upgrades.HyperPerception (
  hyperPerception,
  HyperPerception (..),
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

hyperPerception :: UpgradeCard HyperPerception
hyperPerception = upgrade HyperPerception Cards.hyperPerception

newtype HyperPerception = HyperPerception (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Quicksilver gets +1 THW."
instance HasModifiersFor HyperPerception where
  getModifiersFor _ (IdentityRef ident) (HyperPerception a)
    | ident == upgradeController a = pure [ThwartModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage HyperPerception where
  runMessage msg (HyperPerception attrs) = HyperPerception <$> runMessage msg attrs
