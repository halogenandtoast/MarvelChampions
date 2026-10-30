module Marvel.Upgrade.Upgrades.PhotonBlasters where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Identity (ironheartVersion)
import Marvel.Matchers
import Marvel.Question (runAbility)

photonBlasters :: UpgradeCard PhotonBlasters
photonBlasters = upgrade PhotonBlasters Cards.photonBlasters

newtype PhotonBlasters = PhotonBlasters (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PhotonBlasters where
  getAbilities a = [ability a 1 HeroAction OwnsThis ExhaustCost (runAbility a 1)]

instance HasModifiersFor PhotonBlasters where
  getModifiersFor _ (IdentityRef you) (PhotonBlasters a) = pure [HitPointModifier 2 | you == upgradeController a]
  getModifiersFor _ _ _ = pure []

instance RunMessage PhotonBlasters where
  runMessage msg e@(PhotonBlasters attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      version <- ironheartVersion you
      chooseDamage you attrs (toDamage version FromAbility) AnyEnemy
      pure e
    _ -> PhotonBlasters <$> runMessage msg attrs
