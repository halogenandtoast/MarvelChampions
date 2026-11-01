module Marvel.Ally.Allies.SpiderManPavitrPrabhakar where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Game.Source (countControlledCardsWithTrait)
import Marvel.Trait

spiderManPavitrPrabhakar :: AllyCard SpiderManPavitrPrabhakar
spiderManPavitrPrabhakar = ally SpiderManPavitrPrabhakar Cards.spiderManPavitrPrabhakar (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype SpiderManPavitrPrabhakar = SpiderManPavitrPrabhakar (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpiderManPavitrPrabhakar where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response
    (OwnsThis <> SchemeExists ThwartableScheme) NoCost (runAbility a 1)]

instance RunMessage SpiderManPavitrPrabhakar where
  runMessage msg a@(SpiderManPavitrPrabhakar attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      n <- countControlledCardsWithTrait you WebWarrior
      removeThreat you attrs n ThwartableScheme
      pure a
    _ -> SpiderManPavitrPrabhakar <$> runMessage msg attrs
