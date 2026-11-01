module Marvel.Ally.Allies.Banshee where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

banshee :: AllyCard Banshee
banshee = ally Banshee Cards.banshee (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype Banshee = Banshee (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Banshee where
  getAbilities (Banshee a) =
    [limitedWindowAbility a 1 (W.AllyThwarted W.After (AllyWithId $ allyId a) AnyScheme)
      Response (OwnsThis <> EnemyExists (MinionEnemy <> EnemyCanBeConfused)) NoCost (runAbility a 1)]

instance RunMessage Banshee where
  runMessage msg e@(Banshee attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      minions <- selectList (MinionEnemy <> EnemyCanBeConfused)
      chooseOne you [targeting (enemyRef mid) (giveStatus mid ConfusedStatus) | mid <- minions]
      pure e
    _ -> Banshee <$> runMessage msg attrs
