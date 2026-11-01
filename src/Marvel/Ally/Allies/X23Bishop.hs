module Marvel.Ally.Allies.X23Bishop where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

x23Bishop :: AllyCard X23Bishop
x23Bishop = ally X23Bishop Cards.x23Bishop (Thw 1, 1) (Atk 3, 1) (HP 3)

newtype X23Bishop = X23Bishop (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities X23Bishop where
  getAbilities a@(X23Bishop attrs) =
    [limitedWindowAbility a 1 (W.EnemyDefeated W.After AnyEnemy $ AttackFromAlly $ AllyWithId $ allyId attrs)
      Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage X23Bishop where
  runMessage msg a@(X23Bishop attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ ready (allyId attrs)
    _ -> X23Bishop <$> runMessage msg attrs
