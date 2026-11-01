module Marvel.Ally.Allies.Marrow where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
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

marrow :: AllyCard Marrow
marrow = ally Marrow Cards.marrow (Thw 1, 1) (Atk 2, 1) (HP 2)

newtype Marrow = Marrow (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Marrow where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response (OwnsThis <> EnemyExists AnyEnemy) NoCost (runAbility a 1)]

instance RunMessage Marrow where
  runMessage msg a@(Marrow attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ chooseDamage you attrs (toDamage 2 FromAbility) AnyEnemy
    _ -> Marrow <$> runMessage msg attrs
