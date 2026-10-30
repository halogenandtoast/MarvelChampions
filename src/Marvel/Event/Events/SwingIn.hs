module Marvel.Event.Events.SwingIn where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Resource

swingIn :: EventCard SwingIn
swingIn = event SwingIn Cards.swingIn

newtype SwingIn = SwingIn (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SwingIn where
  runMessage msg e@(SwingIn attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      mental <- paidWith Mental payment
      removeThreat you attrs 4 ThwartableScheme
      when mental $ cardSpecific (IdentityRef you) "spiderCamouflage" ()
      pure e
    _ -> SwingIn <$> runMessage msg attrs
