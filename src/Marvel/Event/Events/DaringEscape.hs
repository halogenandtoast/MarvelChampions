module Marvel.Event.Events.DaringEscape (
  daringEscape,
  DaringEscape (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

daringEscape :: EventCard DaringEscape
daringEscape = event DaringEscape Cards.daringEscape

newtype DaringEscape = DaringEscape (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action, deal yourself 1 facedown encounter card (the def's cost) → ready your
-- hero and draw 1 card.
instance RunMessage DaringEscape where
  runMessage msg e@(DaringEscape attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        ready identityId
        drawCards identityId 1
        pure e
      _ -> DaringEscape <$> runMessage msg attrs
    _ -> DaringEscape <$> runMessage msg attrs
