module Marvel.Ally.Allies.Dazzler where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Window qualified as W


dazzler :: AllyCard Dazzler
dazzler = ally Dazzler Cards.dazzler (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype Dazzler = Dazzler (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Dazzler where
  getAbilities a =
    [ limitedWindowAbility a 1 W.EntersPlayThis Response
        (OwnsThis <> EnemyExists EnemyCanBeConfused) NoCost (runAbility a 1)
    ]

instance RunMessage Dazzler where
  runMessage msg a@(Dazzler attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList EnemyCanBeConfused
      chooseOne you [targeting (enemyRef enemy) (confuse enemy attrs) | enemy <- enemies]
      pure a
    _ -> Dazzler <$> runMessage msg attrs
