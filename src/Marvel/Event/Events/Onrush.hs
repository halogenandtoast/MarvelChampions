module Marvel.Event.Events.Onrush (
  onrush,
  Onrush (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

onrush :: EventCard Onrush
onrush = event Onrush Cards.onrush

newtype Onrush = Onrush (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When a card is revealed from the encounter deck, cancel its effects and discard it,
-- as Warp Reality does: its reveal is taken off the queue, so whatever it is, nothing of
-- it resolves. A treachery's When Revealed and surge, a minion's or side scheme's
-- entering play (and so its incite, When Revealed and surge), an attachment's attaching:
-- revealing the card is its effect (RR Reveal), all cancelled, and it goes to the
-- encounter discard pile (RR Cancel).
instance RunMessage Onrush where
  runMessage msg e@(Onrush attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ (Just (W.EncounterCardRevealed _ _ card))) | ident == eventId attrs -> do
      cancelMatchingMessage \case
        RevealedEncounterCard _ card' -> card' == card
        _ -> False
      discardCard (EncounterCard card)
      pure e
    _ -> Onrush <$> runMessage msg attrs
