module Marvel.Upgrade.Upgrades.CoordinatedAttack where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W
import Marvel.Effect.Types

coordinatedAttack :: UpgradeCard CoordinatedAttack
coordinatedAttack = upgrade CoordinatedAttack Cards.coordinatedAttack

newtype CoordinatedAttack = CoordinatedAttack (Attrs Upgrade)
  deriving anyclass (HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade CoordinatedAttack where
  upgradeTargets _ = Just . map enemyRef <$> selectList
    (MinionEnemy <> NotEnemy (EnemyWithUpgrade $ UpgradeIs Cards.coordinatedAttack))

-- The constant reduction is captured before the attack. Its lasting effect survives
-- the attached minion's defeat, until the ally has taken consequential damage.
instance RunMessage CoordinatedAttack where
  runMessage msg u@(CoordinatedAttack attrs) = runQueueT $ case msg of
    CheckWindows windows -> do
      blanked <- isBlanked attrs
      inPlay <- selectAny $ UpgradeWithId $ upgradeId attrs
      unless (blanked || not inPlay) $ for_
        [aid | W.Window W.When (W.AllyAttack aid enemy) <- windows, Just enemy == upgradeAttachedEnemy attrs]
        \aid -> createEffect Cards.coordinatedAttack attrs (AllyEntity $ AllyWithId aid)
      pure u
    _ -> CoordinatedAttack <$> runMessage msg attrs

newtype CoordinatedAttackEffect = CoordinatedAttackEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

coordinatedAttackEffect :: CardEffect CoordinatedAttackEffect
coordinatedAttackEffect = effectWith CoordinatedAttackEffect Cards.coordinatedAttack
  (modifiersL .~ [AttackConsequentialDamageReduction 1])

instance RunMessage CoordinatedAttackEffect where
  runMessage msg e@(CoordinatedAttackEffect attrs) = runQueueT $ case msg of
    AllyMessage aid (AllyAttackLands enemy _) -> do
      active <- effectValidFor attrs (AllyRef aid)
      alive <- selectAny $ EnemyWithId enemy
      when (active && not alive) $ disableEffect (effectId attrs)
      pure e
    AllyMessage aid (AllyAttackMultipleFinished []) -> do
      whenM (effectValidFor attrs $ AllyRef aid) $ disableEffect (effectId attrs)
      pure e
    AllyMessage aid AllyAttackConsequential -> do
      active <- effectValidFor attrs (AllyRef aid)
      -- The ally's handler has queued its consequential damage before this step.
      when active $ cardSpecific attrs "finishConsequence" ()
      pure e
    CardSpecific (isTarget attrs -> True) "finishConsequence" _ -> e <$ disableEffect (effectId attrs)
    _ -> CoordinatedAttackEffect <$> runMessage msg attrs
