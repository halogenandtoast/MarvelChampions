module Marvel.Upgrade.Upgrades.PhoenixSuit where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Keyword
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

phoenixSuit :: UpgradeCard PhoenixSuit
phoenixSuit = upgrade PhoenixSuit Cards.phoenixSuit

newtype PhoenixSuit = PhoenixSuit (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor PhoenixSuit where
  getModifiersFor _ (IdentityRef you) (PhoenixSuit a) | you == upgradeController a = do
    restrained <- identityMatches (IdentityWithTrait Trait.Restrained) you
    unleashed <- identityMatches (IdentityWithTrait Trait.Unleashed) you
    hero <- identityMatches HeroIdentity you
    pure $ [TraitModifier Trait.Aerial | hero] <> [KeywordModifier Steady | restrained] <> [KeywordModifier (Retaliate 1) | unleashed]
  getModifiersFor _ _ _ = pure []

instance RunMessage PhoenixSuit where
  runMessage msg (PhoenixSuit a) = runQueueT $ PhoenixSuit <$> runMessage msg a
