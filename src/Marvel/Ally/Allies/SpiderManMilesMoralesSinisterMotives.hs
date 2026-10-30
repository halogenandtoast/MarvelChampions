module Marvel.Ally.Allies.SpiderManMilesMoralesSinisterMotives where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Modifier
import Marvel.Ref
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait (Trait (WebWarrior))
import Marvel.Window qualified as W

spiderManMilesMoralesSinisterMotives :: AllyCard SpiderManMilesMoralesSinisterMotives
spiderManMilesMoralesSinisterMotives = ally SpiderManMilesMoralesSinisterMotives Cards.spiderManMilesMoralesSinisterMotives (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype SpiderManMilesMoralesSinisterMotives = SpiderManMilesMoralesSinisterMotives (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpiderManMilesMoralesSinisterMotives where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.PlayedFromHand You $ CardIs Cards.spiderManMilesMoralesSinisterMotives)
        Response (OwnsThis <> ControlsCardsWithTrait WebWarrior 3 <> EnemyExists AnyEnemy) NoCost (runAbility a 1)
    ]

instance RunMessage SpiderManMilesMoralesSinisterMotives where
  runMessage msg a@(SpiderManMilesMoralesSinisterMotives attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList AnyEnemy
      unless (null enemies) $ chooseOne you
        [targeting (enemyRef enemy) (stun enemy attrs >> confuse enemy attrs) | enemy <- enemies]
      pure a
    _ -> SpiderManMilesMoralesSinisterMotives <$> runMessage msg attrs
