module Marvel.Upgrade.Upgrades.Mjolnir (
  mjolnir,
  Mjolnir (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

mjolnir :: UpgradeCard Mjolnir
mjolnir = upgrade Mjolnir Cards.mjolnir

newtype Mjolnir = Mjolnir (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Thor gets +1 ATK and gains the Aerial trait": the hero, not Odinson.
instance HasModifiersFor Mjolnir where
  getModifiersFor _ (IdentityRef ident) (Mjolnir a) | ident == upgradeController a = do
    isHero <- identityMatches HeroIdentity ident
    pure [m | isHero, m <- [AttackModifier 1, TraitModifier Aerial]]
  getModifiersFor _ _ _ = pure []

instance RunMessage Mjolnir where
  runMessage msg (Mjolnir attrs) = Mjolnir <$> runMessage msg attrs
