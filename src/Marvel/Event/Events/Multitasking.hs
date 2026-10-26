module Marvel.Event.Events.Multitasking (
  multitasking,
  Multitasking (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Resource

multitasking :: EventCard Multitasking
multitasking = event Multitasking Cards.multitasking

newtype Multitasking = Multitasking (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (thwart): remove 2 threat from a scheme. Paid for using a [mental]
-- resource, remove 2 threat from a different scheme, chosen after the first thwart.
instance RunMessage Multitasking where
  runMessage msg e@(Multitasking attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      mental <- paidWith Mental payment
      schemes <- selectList ThwartableScheme
      chooseOne you
        [ targeting (schemeRef scheme) do
            thwartScheme scheme attrs 2
            when mental $ cardSpecific attrs "different" (you, scheme)
        | scheme <- schemes
        ]
      pure e
    CardSpecific (isTarget attrs -> True) "different" (fromJSON -> Success (you, first :: SchemeId)) -> do
      others <- filter (/= first) <$> selectList ThwartableScheme
      chooseOne you [targeting (schemeRef scheme) (thwartScheme scheme attrs 2) | scheme <- others]
      pure e
    _ -> Multitasking <$> runMessage msg attrs
