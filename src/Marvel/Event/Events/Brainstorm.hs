module Marvel.Event.Events.Brainstorm (
  brainstorm,
  Brainstorm (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Deck
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref

brainstorm :: EventCard Brainstorm
brainstorm = event Brainstorm Cards.brainstorm

newtype Brainstorm = Brainstorm (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (thwart): name a card type (one a player deck holds), then look at the
-- top card of your deck: of that type, 3 threat is thwarted from the main scheme. The
-- card goes on the top or bottom of your deck, then you draw 1 card.
instance RunMessage Brainstorm where
  runMessage msg e@(Brainstorm attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      chooseOne you [labeled ("$label.name" <> tshow t) (cardSpecific attrs "named" t) | t <- nameable]
      pure e
    CardSpecific (isTarget attrs -> True) "named" (fromJSON -> Success (t :: CardType)) -> do
      let you = eventController attrs
      deck <- projectMap PlayerIdentityDeck unDeck you
      for_ (headMay deck) \top -> lookingAt you [PlayerCard top] do
        when (cdCardType (getCardDef top) == t) do
          schemes <- selectList MainScheme
          for_ (headMay schemes) \s -> thwartScheme s attrs 3
        chooseOne you
          [ labeled "$label.placeOnTopOfDeck" (placeOnTopOfDeck you top)
          , labeled "$label.placeOnBottomOfDeck" (placeOnBottomOfDeck you top)
          ]
      drawCards you 1
      pure e
    _ -> Brainstorm <$> runMessage msg attrs
   where
    -- the card types of a player deck (RR Card Types)
    nameable = [AllyType, EventType, ResourceType, SupportType, UpgradeType]
