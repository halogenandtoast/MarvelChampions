module Marvel.Ally.Allies.SpiderManNoir where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Types
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Trait (Trait (WebWarrior))
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window qualified as W

spiderManNoir :: AllyCard SpiderManNoir
spiderManNoir = ally SpiderManNoir Cards.spiderManNoir (Thw 0, 1) (Atk 0, 1) (HP 3)

newtype SpiderManNoir = SpiderManNoir (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpiderManNoir where
  getAbilities a@(SpiderManNoir attrs) =
    [ limitedWindowAbility a 1 (W.TreacheryResolvedBy You) Response
        (OwnsThis <> ControlsCardsWithTrait WebWarrior 2) NoCost (runAbility a 1)
    | length (allyHeldTreacheries attrs) < 3 ]

instance HasModifiersFor SpiderManNoir where
  getModifiersFor _ target (SpiderManNoir attrs) =
    pure [modifier (fromIntegral $ length $ allyHeldTreacheries attrs)
         | isTarget attrs target, modifier <- [ThwartModifier, AttackModifier]]

instance RunMessage SpiderManNoir where
  runMessage msg a@(SpiderManNoir attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [card | W.ResolvedTreacheryCard you card <- windows, you == allyController attrs] $ \card ->
        when (length (allyHeldTreacheries attrs) < 3) $ attachTreacheryToAlly (allyId attrs) card
      pure a
    _ -> SpiderManNoir <$> runMessage msg attrs
