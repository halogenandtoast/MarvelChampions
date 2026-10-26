module Marvel.Upgrade.Upgrades.WaspsHelmet (
  waspsHelmet,
  WaspsHelmet (..),
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

waspsHelmet :: UpgradeCard WaspsHelmet
waspsHelmet = upgrade WaspsHelmet Cards.waspsHelmet

newtype WaspsHelmet = WaspsHelmet (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | While you are in Giant hero form, you get +1 THW. While you are in Tiny hero form,
-- you get +1 ATK.
instance HasModifiersFor WaspsHelmet where
  getModifiersFor _ (IdentityRef ident) (WaspsHelmet a) | ident == upgradeController a = do
    giant <- identityMatches (HeroIdentity <> IdentityWithTrait Giant) ident
    tiny <- identityMatches (HeroIdentity <> IdentityWithTrait Tiny) ident
    pure $ [ThwartModifier 1 | giant] <> [AttackModifier 1 | tiny]
  getModifiersFor _ _ _ = pure []

instance RunMessage WaspsHelmet where
  runMessage msg (WaspsHelmet attrs) = WaspsHelmet <$> runMessage msg attrs
