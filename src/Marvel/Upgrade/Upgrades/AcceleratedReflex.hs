module Marvel.Upgrade.Upgrades.AcceleratedReflex (
  acceleratedReflex,
  AcceleratedReflex (..),
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

acceleratedReflex :: UpgradeCard AcceleratedReflex
acceleratedReflex = upgrade AcceleratedReflex Cards.acceleratedReflex

newtype AcceleratedReflex = AcceleratedReflex (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Quicksilver gets +1 DEF."
instance HasModifiersFor AcceleratedReflex where
  getModifiersFor _ (IdentityRef ident) (AcceleratedReflex a)
    | ident == upgradeController a = pure [DefenseModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage AcceleratedReflex where
  runMessage msg (AcceleratedReflex attrs) = AcceleratedReflex <$> runMessage msg attrs
