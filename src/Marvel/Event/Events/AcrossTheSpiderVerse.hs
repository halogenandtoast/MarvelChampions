module Marvel.Event.Events.AcrossTheSpiderVerse where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (Choice (UseAbility), runAbility)
import Marvel.Ref
import Marvel.Trait (Trait (WebWarrior))

acrossTheSpiderVerse :: EventCard AcrossTheSpiderVerse
acrossTheSpiderVerse = event AcrossTheSpiderVerse Cards.acrossTheSpiderVerse

newtype AcrossTheSpiderVerse = AcrossTheSpiderVerse (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

repeatCost :: Cost
repeatCost = MultiResourceCost [Nothing, Nothing, Nothing] <> ExhaustCardsWithTraitCost WebWarrior 1 1

instance RunMessage AcrossTheSpiderVerse where
  runMessage msg e@(AcrossTheSpiderVerse attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      cardSpecific attrs "retrieve" you
      pure e
    -- The chosen player resolves the complete ability again, with their own card to
    -- exhaust and their own discard pile. Each repetition can offer another one.
    RanAbility you (isTarget attrs -> True) 1 _ _ -> e <$ cardSpecific attrs "retrieve" you
    CardSpecific (isTarget attrs -> True) "retrieve" (fromJSON -> Success you) -> do
      allies <- selectList (InDiscardOf (IdentityWithId you) (BasicCardMatches $ CardWithType AllyType <> CardWithTrait WebWarrior) <> CanEnterPlay)
      unless (null allies) $ chooseOne you
        [targeting card do
          putIntoPlay you card
          cardSpecific attrs "afterPut" (you, AllyId $ unCardId $ pcCardId card)
        | card <- allies]
      pure e
    CardSpecific (isTarget attrs -> True) "afterPut" (fromJSON -> Success (you, aid)) -> do
      entered <- selectAny (AllyWithId aid)
      when entered do
        players <- selectList AnyIdentity
        unless (null players) $ chooseOne you [targeting p (cardSpecific attrs "offerRepeat" p) | p <- players]
      pure e
    CardSpecific (isTarget attrs -> True) "offerRepeat" (fromJSON -> Success payer) -> do
      affordable <- canAffordCost payer (toSource attrs) repeatCost
      let repeatAbility = limitedAbility attrs 1 NoLimit Action NoCriteria repeatCost (runAbility attrs 1)
      chooseOne payer $
        [labeled "$label.done" (pure ())]
          <> [labeled "$label.pay" (resolveAs payer $ UseAbility repeatAbility) | affordable]
      pure e
    _ -> AcrossTheSpiderVerse <$> runMessage msg attrs
