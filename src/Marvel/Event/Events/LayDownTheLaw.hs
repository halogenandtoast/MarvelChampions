module Marvel.Event.Events.LayDownTheLaw (
  layDownTheLaw,
  LayDownTheLaw (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Payment
import Marvel.Resource

layDownTheLaw :: EventCard LayDownTheLaw
layDownTheLaw = event LayDownTheLaw Cards.layDownTheLaw

newtype LayDownTheLaw = LayDownTheLaw (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response (thwart): After you change form, remove 3 threat from a scheme (4
-- threat instead if you paid for this card using a [mental] resource).
instance RunMessage LayDownTheLaw where
  runMessage msg e@(LayDownTheLaw attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      mental <- paidWith Mental payment
      removeThreat you attrs (if mental then 4 else 3) ThwartableScheme
      pure e
    _ -> LayDownTheLaw <$> runMessage msg attrs
