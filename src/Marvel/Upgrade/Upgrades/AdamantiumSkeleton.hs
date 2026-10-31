module Marvel.Upgrade.Upgrades.AdamantiumSkeleton where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

adamantiumSkeleton :: UpgradeCard AdamantiumSkeleton
adamantiumSkeleton = upgrade AdamantiumSkeleton Cards.adamantiumSkeleton

newtype AdamantiumSkeleton = AdamantiumSkeleton (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor AdamantiumSkeleton where
  getModifiersFor _ (IdentityRef you) (AdamantiumSkeleton attrs)
    | you == upgradeController attrs = do
        wolverine <- identityMatches (HeroIdentity <> IdentityWithTitle "Wolverine") you
        pure $ HitPointModifier 4 : [AttackModifier 1 | wolverine] <> [BasicAttackKeyword Piercing | wolverine]
  getModifiersFor _ _ _ = pure []

instance RunMessage AdamantiumSkeleton where
  runMessage msg (AdamantiumSkeleton attrs) = runQueueT $ AdamantiumSkeleton <$> runMessage msg attrs
