module Marvel.Event.Events.Mulligan where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Projection
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

mulligan :: EventCard Mulligan
mulligan = event Mulligan Cards.mulligan

newtype Mulligan = Mulligan (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Mulligan where
  runMessage msg e@(Mulligan attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      hand <- projectMap PlayerIdentityHand unHand you
      for_ hand discardCard
      drawToHandSize you
      pure e
    _ -> Mulligan <$> runMessage msg attrs
