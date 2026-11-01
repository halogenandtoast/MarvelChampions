module Marvel.Upgrade.Upgrades.AdamantiumLacing where

import Marvel.Prelude
import Marvel.Matchers
import Marvel.Keyword
import Marvel.Query ()
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

adamantiumLacing :: UpgradeCard AdamantiumLacing
adamantiumLacing = upgrade AdamantiumLacing Cards.adamantiumLacing

newtype AdamantiumLacing = AdamantiumLacing (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor AdamantiumLacing where
  getModifiersFor _ (IdentityRef you) (AdamantiumLacing attrs) = do
    isX23 <- identityMatches (IdentityWithTitle "X-23") you
    pure $ [HitPointModifier 2 | you == upgradeController attrs] <>
      [modifier | isX23, modifier <- [KeywordModifier (Retaliate 1), BasicAttackKeyword Piercing]]
  getModifiersFor _ _ _ = pure []

instance RunMessage AdamantiumLacing where
  runMessage msg (AdamantiumLacing attrs) = runQueueT $ AdamantiumLacing <$> runMessage msg attrs
