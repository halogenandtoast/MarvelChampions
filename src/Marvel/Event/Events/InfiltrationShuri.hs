module Marvel.Event.Events.InfiltrationShuri where
import Marvel.Prelude
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source (getEncounterDeck)
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Event.Types

infiltrationShuri :: EventCard InfiltrationShuri
infiltrationShuri = event InfiltrationShuri Cards.infiltrationShuri
newtype InfiltrationShuri = InfiltrationShuri (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage InfiltrationShuri where
  runMessage msg u@(InfiltrationShuri attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      deck <- getEncounterDeck
      let maximumDiscard = min 5 (fromIntegral $ length deck)
      chooseOne you [labeled (countLabel "discardEncounterCards" n) (discardTopOfEncounterDeckFor n attrs) | n <- [1..maximumDiscard]]
      pure u
    WithDiscarded (isTarget attrs -> True) _ cards -> do
      let minions = [card | EncounterCard card <- cards, cdCardType (getCardDef card) == MinionType]
      removeThreat (eventController attrs) attrs (fromIntegral $ length cards) ThwartableScheme
      cardSpecific attrs "minions" minions
      pure u
    CardSpecific (isTarget attrs -> True) "minions" (fromJSON -> Success (cards :: [EncounterCard])) -> do
      let you = eventController attrs
      chooseOne you [targeting card (putEncounterCardIntoPlay you card) | card <- cards]
      pure u
    _ -> InfiltrationShuri <$> runMessage msg attrs
