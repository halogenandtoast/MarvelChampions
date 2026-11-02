module Marvel.Upgrade.Upgrades.ReadyforaFightSilk where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Upgrade.Types

readyforaFightSilk :: UpgradeCard ReadyforaFightSilk
readyforaFightSilk = upgrade ReadyforaFightSilk Cards.readyforaFightSilk
newtype ReadyforaFightSilk = ReadyforaFightSilk (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage ReadyforaFightSilk where
  runMessage msg u@(ReadyforaFightSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [enemy | W.EnemySchemeBegins enemy _ <- windows] $ \enemy -> do
        cancelPendingEnemyScheme enemy
        hero <- identityMatches HeroIdentity you
        unless hero $ changeToHeroForm you
        enemyAttacks enemy you
      pure u
    _ -> ReadyforaFightSilk <$> runMessage msg attrs

instance HasAbilities ReadyforaFightSilk where
  getAbilities u@(ReadyforaFightSilk attrs) = [limitedWindowAbility u 1 (W.EnemyWouldScheme AnyEnemy AnyIdentity) Interrupt OwnsThis (DiscardCost $ toTarget attrs) (runAbility u 1)]
