module Marvel.Event.Events.NaturalFlight where

import Marvel.Prelude
import Marvel.Ability qualified as Ability ()
import Marvel.Card
import Marvel.Card.Side ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Window qualified as W ()

naturalFlight :: EventCard NaturalFlight
naturalFlight = event NaturalFlight Cards.naturalFlight

newtype NaturalFlight = NaturalFlight (Attrs Event)
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor NaturalFlight where
  getModifiersFor _ target (NaturalFlight attrs) | target == toRef attrs = do
    angel <- identityMatches (IdentityWithTitle "Angel") (eventController attrs)
    pure $ if angel then [IgnoresCrisis, IgnoresPatrol] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage NaturalFlight where
  runMessage msg e@(NaturalFlight attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      angel <- identityMatches (IdentityWithTitle "Angel") you
      removeThreat you attrs 4 (if angel then ThwartableSchemeIgnoringCrisis else SchemeYouCanThwart)
      pure e
    _ -> NaturalFlight <$> runMessage msg attrs
