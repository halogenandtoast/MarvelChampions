module Marvel.Upgrade.Upgrades.HeroicIntuition
  ( heroicIntuition
  , HeroicIntuition(..)
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

heroicIntuition :: UpgradeCard HeroicIntuition
heroicIntuition = upgrade HeroicIntuition Cards.heroicIntuition

newtype HeroicIntuition = HeroicIntuition UpgradeAttrs
  deriving anyclass IsUpgrade
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor HeroicIntuition where
  getModifiersFor _ (IdentityTarget iid) (HeroicIntuition a)
    | iid == upgradeController a = pure [ThwartModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities HeroicIntuition where
  getAbilities _ = []

instance RunMessage HeroicIntuition where
  runMessage msg (HeroicIntuition attrs) =
    HeroicIntuition <$> runMessage msg attrs
