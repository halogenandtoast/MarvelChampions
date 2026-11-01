module Marvel.Ally.Allies.Shatterstar where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

shatterstar :: AllyCard Shatterstar
shatterstar = ally Shatterstar Cards.shatterstar (Thw 2, 2) (Atk 2, 1) (HP 4)

newtype Shatterstar = Shatterstar (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Shatterstar where
  getAbilities a@(Shatterstar attrs) = [limitedWindowAbility a 1
    (W.AllyAttacked W.When (AllyWithId $ allyId attrs) MinionEnemy)
    Interrupt OwnsThis NoCost (runAbility a 1)]

instance RunMessage Shatterstar where
  runMessage msg a@(Shatterstar attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [mid | W.AllyAttack _ (EnemyMinionId mid) <- windows] $ \mid -> do
        already <- selectAny (MinionWithId mid <> MinionEngagedWith (IdentityWithId you))
        if already then raiseAllyAttack (allyId attrs) 1 else engageMinion mid you
      pure a
    _ -> Shatterstar <$> runMessage msg attrs
