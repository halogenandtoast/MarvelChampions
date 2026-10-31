module Marvel.Ally.Allies.Longshot where

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
import Marvel.Resource
import Marvel.Stats
import Marvel.Keyword
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Boost (BoostIcon (Star))
import Marvel.Trait qualified as Trait

longshot :: AllyCard Longshot
longshot = ally Longshot Cards.longshot (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype Longshot = Longshot (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Longshot where
  getAbilities (Longshot a) =
    [limitedWindowAbility a 1 (W.AllyAttacked W.After (AllyWithId $ allyId a)
      (MinionEnemy <> NotEnemy (EnemyWithTrait Trait.Elite))) Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage Longshot where
  runMessage msg e@(Longshot attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [mid | W.AllyAttack _ (EnemyMinionId mid) <- windows] \mid ->
        discardTopOfEncounterDeckThen 1 \cards ->
          when (any (elem Star . cdBoostIcons . getCardDef) cards) (defeatMinion mid)
      pure e
    _ -> Longshot <$> runMessage msg attrs
