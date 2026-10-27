module Marvel.Event.Events.GrandStrategy (
  grandStrategy,
  GrandStrategy (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Id (CardId (..))
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

grandStrategy :: EventCard GrandStrategy
grandStrategy = event GrandStrategy Cards.grandStrategy

newtype GrandStrategy = GrandStrategy (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Draw up to your maximum hand size (as modified, as at the end of the player phase),
-- then it is removed from the game, not discarded.
instance RunMessage GrandStrategy where
  runMessage msg e@(GrandStrategy attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent you _ _ -> do
        drawToHandSize you
        pure e
      ResolvedEvent -> do
        removeFromGame (CardIdRef . CardId . unEventId $ eventId attrs)
        pure e
      _ -> GrandStrategy <$> runMessage msg attrs
    _ -> GrandStrategy <$> runMessage msg attrs
