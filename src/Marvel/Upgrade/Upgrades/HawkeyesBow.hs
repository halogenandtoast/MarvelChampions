module Marvel.Upgrade.Upgrades.HawkeyesBow (
  hawkeyesBow,
  HawkeyesBow (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

hawkeyesBow :: UpgradeCard HawkeyesBow
hawkeyesBow = upgrade HawkeyesBow Cards.hawkeyesBow

newtype HawkeyesBow = HawkeyesBow (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Restricted. "Your hero gets +1 ATK and each of your Arrow attacks gain ranged": the
-- ranged is read as an Arrow attack deals its damage ('arrowDamage').
instance HasModifiersFor HawkeyesBow where
  getModifiersFor _ (IdentityRef ident) (HawkeyesBow a) | ident == upgradeController a = do
    isHero <- identityMatches HeroIdentity ident
    pure [AttackModifier 1 | isHero]
  getModifiersFor _ _ _ = pure []

instance RunMessage HawkeyesBow where
  runMessage msg (HawkeyesBow attrs) = HawkeyesBow <$> runMessage msg attrs
