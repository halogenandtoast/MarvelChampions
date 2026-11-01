module Marvel.Event.Events.Scrying where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Card
import Marvel.Deck
import Marvel.Identity.Types
import Marvel.Projection

scrying :: EventCard Scrying
scrying = event Scrying Cards.scrying

newtype Scrying = Scrying (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Scrying where
  runMessage msg e@(Scrying attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      cards <- take 3 <$> projectMap PlayerIdentityDeck unDeck you
      lookingAt you (map PlayerCard cards) $
        chooseOne you [targeting card do
          placeOnTopOfDeck you card
          drawCards you 1
          cardSpecific attrs "discard" (you, filter (/= card) cards)
          | card <- cards]
      pure e
    CardSpecific (isTarget attrs -> True) "discard" (fromJSON -> Success (you, cards :: [PlayerCard])) -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      chooseOne you [targeting card do
        placeOnTopOfDeck you card
        discardFromDeck you 1 attrs
        cardSpecific attrs "return" (you, filter (/= card) cards)
        | card <- cards, card `elem` deck]
      pure e
    CardSpecific (isTarget attrs -> True) "return" (fromJSON -> Success (you, cards :: [PlayerCard])) -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      for_ [card | card <- cards, card `elem` deck] (placeOnTopOfDeck you)
      pure e
    _ -> Scrying <$> runMessage msg attrs
