module Marvel.Upgrade.Upgrades.RedRoomTraining (
  redRoomTraining,
  RedRoomTraining (..),
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
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

redRoomTraining :: UpgradeCard RedRoomTraining
redRoomTraining = upgrade RedRoomTraining Cards.redRoomTraining

newtype RedRoomTraining = RedRoomTraining (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | While you are in Giant hero form, you gain retaliate 1. While you are in Tiny hero
-- form, your basic attacks gain piercing.
instance HasModifiersFor RedRoomTraining where
  getModifiersFor _ (IdentityRef ident) (RedRoomTraining a) | ident == upgradeController a = do
    giant <- identityMatches (HeroIdentity <> IdentityWithTrait Giant) ident
    tiny <- identityMatches (HeroIdentity <> IdentityWithTrait Tiny) ident
    pure $ [KeywordModifier (Retaliate 1) | giant] <> [KeywordModifier Piercing | tiny]
  getModifiersFor _ _ _ = pure []

instance RunMessage RedRoomTraining where
  runMessage msg (RedRoomTraining attrs) = RedRoomTraining <$> runMessage msg attrs
