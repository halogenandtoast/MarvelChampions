module Marvel.Upgrade.Upgrades.SideHolster (
  sideHolster,
  SideHolster (..),
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

sideHolster :: UpgradeCard SideHolster
sideHolster = upgrade SideHolster Cards.sideHolster

newtype SideHolster = SideHolster (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "You can control 1 additional Weapon upgrade that has the restricted keyword": its
-- controller, whoever's control it was played under.
instance HasModifiersFor SideHolster where
  getModifiersFor _ (IdentityRef ident) (SideHolster a)
    | ident == upgradeController a = pure [RestrictedWeaponLimitModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage SideHolster where
  runMessage msg (SideHolster attrs) = SideHolster <$> runMessage msg attrs
