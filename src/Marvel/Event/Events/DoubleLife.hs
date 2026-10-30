module Marvel.Event.Events.DoubleLife where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Card.Side
import Marvel.Matchers
import Marvel.Payment
import Marvel.Resource

doubleLife :: EventCard DoubleLife
doubleLife = event DoubleLife Cards.doubleLife

newtype DoubleLife = DoubleLife (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage DoubleLife where
  runMessage msg e@(DoubleLife attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      physical <- paidWith Physical payment
      hero <- identityMatches HeroIdentity you
      changeToForm you (if hero then B else A)
      when physical (ready you)
      pure e
    _ -> DoubleLife <$> runMessage msg attrs
