module Marvel.Upgrade.Upgrades.CyberneticSkeleton (
  cyberneticSkeleton,
  CyberneticSkeleton (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

cyberneticSkeleton :: UpgradeCard CyberneticSkeleton
cyberneticSkeleton = upgrade CyberneticSkeleton Cards.cyberneticSkeleton

newtype CyberneticSkeleton = CyberneticSkeleton (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | You get +3 hit points. While in hero form, Rocket Raccoon gets +1 ATK.
instance HasModifiersFor CyberneticSkeleton where
  getModifiersFor _ (IdentityRef ident) (CyberneticSkeleton a) | ident == upgradeController a = do
    isHero <- identityMatches HeroIdentity ident
    pure $ HitPointModifier 3 : [AttackModifier 1 | isHero]
  getModifiersFor _ _ _ = pure []

instance RunMessage CyberneticSkeleton where
  runMessage msg (CyberneticSkeleton attrs) = CyberneticSkeleton <$> runMessage msg attrs
