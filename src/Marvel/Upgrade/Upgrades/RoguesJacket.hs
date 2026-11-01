module Marvel.Upgrade.Upgrades.RoguesJacket where

import Marvel.Id
import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Ability
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Matchers ()
import Marvel.Query ()
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

roguesJacket :: UpgradeCard RoguesJacket
roguesJacket = upgrade RoguesJacket Cards.roguesJacket

newtype RoguesJacket = RoguesJacket (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor RoguesJacket where
  getModifiersFor _ (IdentityRef you) (RoguesJacket a) | you == upgradeController a = do
    host <- touchedHost you
    case host of
      Just (IdentityCharacter _) -> pure [ThwartModifier 1]
      Just (AllyCharacter _) -> pure [ThwartModifier 1]
      Just (MinionCharacter _) -> pure [AttackModifier 1]
      Just (VillainCharacter _) -> pure [AttackModifier 1]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage RoguesJacket where
  runMessage msg (RoguesJacket attrs) = runQueueT $ RoguesJacket <$> runMessage msg attrs
