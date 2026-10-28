module Marvel.Event.Events.ZoneOfSilence (
  zoneOfSilence,
  ZoneOfSilence (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref

zoneOfSilence :: EventCard ZoneOfSilence
zoneOfSilence = event ZoneOfSilence Cards.zoneOfSilence

newtype ZoneOfSilence = ZoneOfSilence (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (thwart): Choose a scheme and discard up to 4 cards from the top of your
-- deck (its cost) → remove 1 threat from that scheme for each card discarded this way.
instance RunMessage ZoneOfSilence where
  runMessage msg e@(ZoneOfSilence attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      removeThreat you attrs (fromIntegral $ length (paymentDiscards payment)) ThwartableScheme
      pure e
    _ -> ZoneOfSilence <$> runMessage msg attrs
