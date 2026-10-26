module Marvel.Event.Events.SpiritualMeditation (
  spiritualMeditation,
  SpiritualMeditation (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

spiritualMeditation :: EventCard SpiritualMeditation
spiritualMeditation = event SpiritualMeditation Cards.spiritualMeditation

newtype SpiritualMeditation = SpiritualMeditation (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Action (Mystic only, its criteria): draw 2 cards, then choose and discard 1.
instance RunMessage SpiritualMeditation where
  runMessage msg e@(SpiritualMeditation attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      drawCards you 2
      discardFromHand you 1
      pure e
    _ -> SpiritualMeditation <$> runMessage msg attrs
