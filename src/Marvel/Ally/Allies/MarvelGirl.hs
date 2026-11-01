module Marvel.Ally.Allies.MarvelGirl where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Projection ()
import Marvel.Minion.Types ()

marvelGirl :: AllyCard MarvelGirl
marvelGirl = ally MarvelGirl Cards.marvelGirl (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype MarvelGirl = MarvelGirl (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MarvelGirl where
  getAbilities (MarvelGirl a) =
    [limitedWindowAbility a 1 (W.AllyAttacked W.When (AllyWithId $ allyId a) MinionEnemy)
      Interrupt (OwnsThis <> SchemeExists (MainScheme <> ThwartableScheme)) NoCost (runAbility a 1)]

instance RunMessage MarvelGirl where
  runMessage msg e@(MarvelGirl attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [mid | W.AllyAttack _ (EnemyMinionId mid) <- windows] \mid -> do
        sch <- getEnemyPrintedScheme (EnemyMinionId mid)
        removeThreat you attrs sch (MainScheme <> ThwartableScheme)
      pure e
    _ -> MarvelGirl <$> runMessage msg attrs
