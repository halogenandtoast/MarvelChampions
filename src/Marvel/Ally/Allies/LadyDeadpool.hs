module Marvel.Ally.Allies.LadyDeadpool where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Stats ()

ladyDeadpool :: AllyCard LadyDeadpool
ladyDeadpool = ally LadyDeadpool Cards.ladyDeadpool (Thw 2, 0) (Atk 2, 0) (HP 3)

newtype LadyDeadpool = LadyDeadpool (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LadyDeadpool where
  getAbilities a@(LadyDeadpool attrs) =
    [limitedWindowAbility a 1 (W.AllyDefeated W.When $ AllyWithId $ allyId attrs)
      ForcedInterrupt OwnsThis NoCost (runAbility a 1)]


instance RunMessage LadyDeadpool where
  runMessage msg e@(LadyDeadpool attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      minions <- selectList (AnyMinion <> NotMinion (MinionWithTrait Trait.Elite))
      chooseOne you [targeting (enemyRef mid) (defeatMinion mid) | mid <- minions]
      pure e
    _ -> LadyDeadpool <$> runMessage msg attrs
