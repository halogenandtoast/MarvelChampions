module Marvel.Ally.Allies.Siryn where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Damage
import Marvel.Trait qualified as Trait
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

siryn :: AllyCard Siryn
siryn = ally Siryn Cards.siryn (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype Siryn = Siryn (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Siryn where
  getAbilities a@(Siryn attrs) = [limitedWindowAbility a 1
    (W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy)
    Response (OwnsThis <> MinionExists MinionCanBeStunned) NoCost (runAbility a 1)]

instance RunMessage Siryn where
  runMessage msg a@(Siryn attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      minions <- selectList MinionCanBeStunned
      chooseOne you [targeting (enemyRef mid) (stun mid attrs) | mid <- minions]
      pure a
    _ -> Siryn <$> runMessage msg attrs
