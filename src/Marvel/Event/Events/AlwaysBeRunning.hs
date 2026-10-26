module Marvel.Event.Events.AlwaysBeRunning (
  alwaysBeRunning,
  AlwaysBeRunning (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

alwaysBeRunning :: EventCard AlwaysBeRunning
alwaysBeRunning = event AlwaysBeRunning Cards.alwaysBeRunning

newtype AlwaysBeRunning = AlwaysBeRunning (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Ready Quicksilver. Played only while he is exhausted (its criteria).
instance RunMessage AlwaysBeRunning where
  runMessage msg e@(AlwaysBeRunning attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      ready you
      pure e
    _ -> AlwaysBeRunning <$> runMessage msg attrs
