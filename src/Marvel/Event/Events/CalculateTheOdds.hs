module Marvel.Event.Events.CalculateTheOdds (
  calculateTheOdds,
  CalculateTheOdds (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

calculateTheOdds :: EventCard CalculateTheOdds
calculateTheOdds = event CalculateTheOdds Cards.calculateTheOdds

newtype CalculateTheOdds = CalculateTheOdds (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: draw 1 card and choose a player (you too). That player may draw 1
-- card, then choose and discard 1 card from their hand: the "may" is the whole of it,
-- the discard following only the draw.
instance RunMessage CalculateTheOdds where
  runMessage msg e@(CalculateTheOdds attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      drawCards you 1
      players <- selectList AnyIdentity
      chooseOne you [targeting p (cardSpecific attrs "player" p) | p <- players]
      pure e
    CardSpecific (isTarget attrs -> True) "player" (fromJSON -> Success (p :: IdentityId)) -> do
      chooseOne p
        [ labeled "$label.drawThenDiscard" do
            drawCards p 1
            discardFromHand p 1
        , labeled "$label.pass" (pure ())
        ]
      pure e
    _ -> CalculateTheOdds <$> runMessage msg attrs
