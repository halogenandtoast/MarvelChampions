module Marvel.Event.Events.Resize (
  resize,
  Resize (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref

resize :: EventCard Resize
resize = event Resize Cards.resize

newtype Resize = Resize (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Change to your other hero form. Draw 1 card.
instance RunMessage Resize where
  runMessage msg e@(Resize attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      changeToHeroForm you
      drawCards you 1
      pure e
    _ -> Resize <$> runMessage msg attrs
