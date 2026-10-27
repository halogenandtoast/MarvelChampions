module Marvel.Event.Events.GutsyMove (
  gutsyMove,
  GutsyMove (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Identity.Types (Field (PlayerIdentityEncounterCards))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref

gutsyMove :: EventCard GutsyMove
gutsyMove = event GutsyMove Cards.gutsyMove

newtype GutsyMove = GutsyMove (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove 2 threat from a scheme, and 2 more from it for each facedown encounter card in
-- front of you: one removal of the total.
instance RunMessage GutsyMove where
  runMessage msg e@(GutsyMove attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        facedown <- length <$> project PlayerIdentityEncounterCards identityId
        removeThreat identityId attrs (2 + 2 * fromIntegral facedown) ThwartableScheme
        pure e
      _ -> GutsyMove <$> runMessage msg attrs
    _ -> GutsyMove <$> runMessage msg attrs
