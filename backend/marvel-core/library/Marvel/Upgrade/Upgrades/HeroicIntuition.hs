module Marvel.Upgrade.Upgrades.HeroicIntuition (
  heroicIntuition,
  HeroicIntuition (..),
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

heroicIntuition :: UpgradeCard HeroicIntuition
heroicIntuition = upgrade HeroicIntuition Cards.heroicIntuition

newtype HeroicIntuition = HeroicIntuition (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasModifiersFor HeroicIntuition where
  getModifiersFor _ (IdentityRef iid) (HeroicIntuition a)
    | iid == upgradeController a = pure [ThwartModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities HeroicIntuition where
  getAbilities _ = []

instance RunMessage HeroicIntuition where
  runMessage msg (HeroicIntuition attrs) =
    HeroicIntuition <$> runMessage msg attrs
