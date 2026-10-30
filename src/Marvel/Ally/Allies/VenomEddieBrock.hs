module Marvel.Ally.Allies.VenomEddieBrock where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

venomEddieBrock :: AllyCard VenomEddieBrock
venomEddieBrock = ally VenomEddieBrock Cards.venomEddieBrock (Thw 2, 1) (Atk 3, 1) (HP 6)

newtype VenomEddieBrock = VenomEddieBrock (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities VenomEddieBrock where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.WindowOneOf
        [ W.EncounterCardRevealedBy W.After You EncounterCardWithBoostIcons
        , W.EncounterCardRevealedBy W.After You EncounterCardWithBoostAbility ])
        Response (OwnsThis <> EnemyExists AnyEnemy) (DamageThisCost 1) (runAbility a 1)
    ]

instance RunMessage VenomEddieBrock where
  runMessage msg a@(VenomEddieBrock attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      let cards = [c | W.EncounterCardResolved _ c <- windows]
      for_ (take 1 cards) $ \card -> do
        enemies <- selectList AnyEnemy
        let n = fromIntegral $ length $ cdBoostIcons $ getCardDef card
        unless (null enemies) $ chooseOne you
          [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage n FromAbility) | enemy <- enemies]
      pure a
    _ -> VenomEddieBrock <$> runMessage msg attrs
