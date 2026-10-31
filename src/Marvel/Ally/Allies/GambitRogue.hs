module Marvel.Ally.Allies.GambitRogue where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

gambitRogue :: AllyCard GambitRogue
gambitRogue = allyWith GambitRogue Cards.gambitRogue (Thw 2, 1) (Atk 2, 1) (HP 3) (countersL .~ 3)

newtype GambitRogue = GambitRogue (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GambitRogue where
  getAbilities a@(GambitRogue attrs) =
    [ limitedWindowAbility a 1 (W.AllyAttacked W.When (AllyWithId $ allyId attrs) AnyEnemy)
        Interrupt (OwnsThis <> EnemyExists (EnemyCanTakeDamageFrom $ toSource attrs)) UseCost (runAbility a 1)
    ]

instance RunMessage GambitRogue where
  runMessage msg a@(GambitRogue attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList (EnemyCanTakeDamageFrom $ toSource attrs)
      chooseOne you [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage 1 FromAbility) | enemy <- enemies]
      pure a
    _ -> GambitRogue <$> runMessage msg attrs
