module Marvel.Ally.Allies.LadySpider where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Game.Source (countControlledCardsWithTrait)
import Marvel.Trait

ladySpider :: AllyCard LadySpider
ladySpider = ally LadySpider Cards.ladySpider (Thw 2, 2) (Atk 2, 2) (HP 4)

newtype LadySpider = LadySpider (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LadySpider where
  getAbilities a@(LadySpider attrs) =
    [limitedWindowAbility a 1 (W.ThreatRemovedByThwart W.After (SourceIs $ toSource attrs) AnyScheme)
      Response (OwnsThis <> ControlsCardsWithTrait WebWarrior 2 <> HasOtherSchemeForThreatRemoval) NoCost (runAbility a 1)]

instance RunMessage LadySpider where
  runMessage msg a@(LadySpider attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [(sid,n) | W.ThwartRemovedThreat source sid _ n <- windows, source == toSource attrs] $ \(sid,n) -> do
        schemes <- filter (/= sid) <$> selectList ThwartableScheme
        unless (null schemes) $ chooseOne you
          [targeting (schemeRef scheme) (thwartScheme scheme attrs n) | scheme <- schemes]
      pure a
    _ -> LadySpider <$> runMessage msg attrs
