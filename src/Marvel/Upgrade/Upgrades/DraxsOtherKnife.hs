module Marvel.Upgrade.Upgrades.DraxsOtherKnife (
  draxsOtherKnife,
  DraxsOtherKnife (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

draxsOtherKnife :: UpgradeCard DraxsOtherKnife
draxsOtherKnife = upgrade DraxsOtherKnife Cards.draxsOtherKnife

newtype DraxsOtherKnife = DraxsOtherKnife (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "While in hero form, Drax gains retaliate 1." (both faces are titled Drax: his identity).
instance HasModifiersFor DraxsOtherKnife where
  getModifiersFor _ (IdentityRef ident) (DraxsOtherKnife a) | ident == upgradeController a = do
    isHero <- identityMatches HeroIdentity ident
    pure [m | isHero, m <- [KeywordModifier (Retaliate 1)]]
  getModifiersFor _ _ _ = pure []

instance RunMessage DraxsOtherKnife where
  runMessage msg (DraxsOtherKnife attrs) = DraxsOtherKnife <$> runMessage msg attrs
