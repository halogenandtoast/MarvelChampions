module Marvel.Ally.Allies.TheLocustFernandaRodriguez where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Aspect
import Marvel.Card.Def ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Window qualified as W

theLocustFernandaRodriguez :: AllyCard TheLocustFernandaRodriguez
theLocustFernandaRodriguez = ally TheLocustFernandaRodriguez Cards.theLocustFernandaRodriguez (Thw 1, 2) (Atk 1, 1) (HP 2)

newtype TheLocustFernandaRodriguez = TheLocustFernandaRodriguez (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TheLocustFernandaRodriguez where
  getAbilities a@(TheLocustFernandaRodriguez attrs) =
    [ limitedWindowAbility a 1 W.EntersPlayThis HeroResponse
        (OwnsThis <> ExtendedCardExists (InDiscardOf (IdentityWithId $ allyController attrs) $ BasicCardMatches aggressionEvent))
        NoCost (runAbility a 1)
    ]

aggressionEvent :: CardMatcher
aggressionEvent = CardWithAspect Aggression <> CardWithType EventType

instance RunMessage TheLocustFernandaRodriguez where
  runMessage msg a@(TheLocustFernandaRodriguez attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches aggressionEvent)
      chooseOne you [targeting card (addToHand you card) | card <- cards]
      pure a
    _ -> TheLocustFernandaRodriguez <$> runMessage msg attrs
