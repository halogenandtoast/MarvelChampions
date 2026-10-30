module Marvel.Ally.Allies.SpiderManHobieBrown where

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
import Marvel.Damage
import Marvel.Matchers
import Marvel.Window qualified as W

spiderManHobieBrown :: AllyCard SpiderManHobieBrown
spiderManHobieBrown = ally SpiderManHobieBrown Cards.spiderManHobieBrown (Thw 2, 2) (Atk 1, 1) (HP 3)

newtype SpiderManHobieBrown = SpiderManHobieBrown (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpiderManHobieBrown where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.AllyLeavesPlay W.When You $ CardIs Cards.spiderManHobieBrown)
        Interrupt OwnsThis NoCost (runAbility a 1)
    ]

instance RunMessage SpiderManHobieBrown where
  runMessage msg a@(SpiderManHobieBrown attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      discardTopOfEncounterDeckThen 3 (countBoostIcons attrs ())
      pure a
    WithBoostIcons (isTarget attrs -> True) _ (sum . map snd -> n) -> do
      withTheVillain (allyController attrs) $ \villain -> dealDamage villain attrs (toDamage n FromAbility)
      pure a
    _ -> SpiderManHobieBrown <$> runMessage msg attrs
