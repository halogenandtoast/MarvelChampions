module Marvel.Upgrade.Upgrades.NowImMad where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

nowImMad :: UpgradeCard NowImMad
nowImMad = upgrade NowImMad Cards.nowImMad

newtype NowImMad = NowImMad (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor NowImMad where
  getModifiersFor _ (IdentityRef you) (NowImMad attrs) | you == upgradeController attrs = do
    hero <- identityMatches HeroIdentity you
    (remaining, _, starting) <- getRemainingHitPoints you
    pure $ if hero && 2 * remaining < starting then [AttackModifier 1, ThwartPenalty 1] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage NowImMad where
  runMessage msg a@(NowImMad attrs) = runQueueT $ case msg of
    _ -> NowImMad <$> runMessage msg attrs
