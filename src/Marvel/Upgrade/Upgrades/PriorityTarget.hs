module Marvel.Upgrade.Upgrades.PriorityTarget where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

priorityTarget :: UpgradeCard PriorityTarget
priorityTarget = upgrade PriorityTarget Cards.priorityTarget

newtype PriorityTarget = PriorityTarget (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade PriorityTarget where
  upgradeTargets _ = Just . map enemyRef <$> selectList (NotEnemy $ EnemyWithUpgrade $ UpgradeIs Cards.priorityTarget)

instance HasAbilities PriorityTarget where
  getAbilities a@(PriorityTarget attrs) =
    [limitedWindowAbility a 1 (W.EnemyDefeated W.When (EnemyWithId enemy) AnyDamage)
      Interrupt OwnsThis NoCost (runAbility a 1) | Just enemy <- [upgradeAttachedEnemy attrs]]

instance RunMessage PriorityTarget where
  runMessage msg u@(PriorityTarget attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      let defeats = [(EnemyMinionId mid, damage, source) | W.DefeatedMinion mid damage source _ <- windows]
            <> [(EnemyVillainId vid, damage, source) | W.DefeatedVillain vid damage source _ <- windows]
      for_ [(damage, source) | (enemy, damage, source) <- defeats, Just enemy == upgradeAttachedEnemy attrs] $ \(damage, source) -> do
        controller <- getSourceController source
        let byAttack = case damageSource damage of
              FromPlayerAttack player -> Just player
              _ -> Nothing
        for_ (controller <|> byAttack) (`drawCards` 2)
      pure u
    _ -> PriorityTarget <$> runMessage msg attrs
