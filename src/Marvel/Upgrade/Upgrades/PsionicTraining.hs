module Marvel.Upgrade.Upgrades.PsionicTraining where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Payment ()
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword ()
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

psionicTraining :: UpgradeCard PsionicTraining
psionicTraining = upgrade PsionicTraining Cards.psionicTraining

newtype PsionicTraining = PsionicTraining (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor PsionicTraining where
  getModifiersFor _ (IdentityRef you) (PsionicTraining a) | you == upgradeController a = do
    psylocke <- identityMatches (HeroIdentity <> IdentityWithTitle "Psylocke") you
    pure $ if psylocke then [IgnoresGuard, IgnoresPatrol] else []
  getModifiersFor _ _ _ = pure []

instance HasAbilities PsionicTraining where
  getAbilities (PsionicTraining a) = [limitedWindowAbility a 1 (W.HeroThwarted W.After You) HeroResponse (OwnsThis <> SelfMatches (IdentityWithTitle "Psylocke") <> EnemyExists AnyEnemy) (DiscardCost $ toTarget a) (runAbility a 1)]

instance RunMessage PsionicTraining where
  runMessage msg e@(PsionicTraining a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      removeEnemies <- selectList AnyEnemy
      chooseOne you [targeting (enemyRef enemy) (confuse enemy a) | enemy <- removeEnemies]
      pure e
    _ -> PsionicTraining <$> runMessage msg a
