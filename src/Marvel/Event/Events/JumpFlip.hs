module Marvel.Event.Events.JumpFlip where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

jumpFlip :: EventCard JumpFlip
jumpFlip = event JumpFlip Cards.jumpFlip

newtype JumpFlip = JumpFlip (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage JumpFlip where
  runMessage msg e@(JumpFlip attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment (Just (W.IdentityTakeDamage hero _))) | eid == eventId attrs -> do
      energy <- paidWith Energy payment
      preventDamage hero 2
      when energy $ removeThreat you attrs 2 MainScheme
      pure e
    _ -> JumpFlip <$> runMessage msg attrs
