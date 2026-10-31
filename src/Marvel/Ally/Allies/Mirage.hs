module Marvel.Ally.Allies.Mirage where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

mirage :: AllyCard Mirage
mirage = ally Mirage Cards.mirage (Thw 2, 1) (Atk 1, 1) (HP 2)

newtype Mirage = Mirage (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mirageEnemies :: HasGame m => Attrs Ally -> m [EnemyId]
mirageEnemies a = do
  thw <- getModifiedThwart a
  filterM (\enemy -> (< thw) <$> getEnemyScheme enemy) =<< selectList EnemyCanBeStunned

instance HasAbilities Mirage where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response (OwnsThis <> EnemyExists (EnemyCanBeStunned <> EnemyWithSchemeLessThanAllyThwart (allyId $ toAllyAttrs a))) NoCost (runAbility a 1)]

instance RunMessage Mirage where
  runMessage msg a@(Mirage attrs) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      enemies <- mirageEnemies attrs
      chooseOne you [targeting (enemyRef enemy) (stun enemy attrs) | enemy <- enemies]
      pure a
    _ -> Mirage <$> runMessage msg attrs
