module Marvel.Upgrade.Upgrades.ThorsHelmet (
  thorsHelmet,
  ThorsHelmet (..),
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

thorsHelmet :: UpgradeCard ThorsHelmet
thorsHelmet = upgrade ThorsHelmet Cards.thorsHelmet

newtype ThorsHelmet = ThorsHelmet (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ThorsHelmet where
  getModifiersFor _ (IdentityRef ident) (ThorsHelmet a)
    | ident == upgradeController a = pure [HitPointModifier 5]
  getModifiersFor _ _ _ = pure []

instance RunMessage ThorsHelmet where
  runMessage msg (ThorsHelmet attrs) = ThorsHelmet <$> runMessage msg attrs
