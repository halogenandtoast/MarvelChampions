module Marvel.Upgrade.Upgrades.DraxsKnife (
  draxsKnife,
  DraxsKnife (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

draxsKnife :: UpgradeCard DraxsKnife
draxsKnife = upgrade DraxsKnife Cards.draxsKnife

newtype DraxsKnife = DraxsKnife (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "While in hero form, Drax gets +1 ATK." (both faces are titled Drax: his identity).
instance HasModifiersFor DraxsKnife where
  getModifiersFor _ (IdentityRef ident) (DraxsKnife a) | ident == upgradeController a = do
    isHero <- identityMatches HeroIdentity ident
    pure [m | isHero, m <- [AttackModifier 1]]
  getModifiersFor _ _ _ = pure []

instance RunMessage DraxsKnife where
  runMessage msg (DraxsKnife attrs) = DraxsKnife <$> runMessage msg attrs
