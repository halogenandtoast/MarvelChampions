module Marvel.Upgrade.Upgrades.Dauntless (
  dauntless,
  Dauntless (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source (getRemainingHitPoints)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

dauntless :: UpgradeCard Dauntless
dauntless = upgrade Dauntless Cards.dauntless

newtype Dauntless = Dauntless (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | While your hero's remaining hit points are equal to or greater than your hero's
-- starting (printed) hit points, your hero gains retaliate 1. "Your" is its controller.
instance HasModifiersFor Dauntless where
  getModifiersFor _ (IdentityRef ident) (Dauntless a)
    | ident == upgradeController a = do
        hero <- identityMatches HeroIdentity ident
        (remaining, _, printed) <- getRemainingHitPoints ident
        pure [KeywordModifier (Retaliate 1) | hero, remaining >= printed]
  getModifiersFor _ _ _ = pure []

instance RunMessage Dauntless where
  runMessage msg (Dauntless attrs) = runQueueT $ case msg of
    _ -> Dauntless <$> runMessage msg attrs
