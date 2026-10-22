module Marvel.Event.Events.SubOrbitalLeap (
  subOrbitalLeap,
  SubOrbitalLeap (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref
import Marvel.Resource

subOrbitalLeap :: EventCard SubOrbitalLeap
subOrbitalLeap = event SubOrbitalLeap Cards.subOrbitalLeap

newtype SubOrbitalLeap = SubOrbitalLeap (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SubOrbitalLeap where
  runMessage msg e@(SubOrbitalLeap attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId payment _ -> do
        physical <- paidUsingOnly Physical payment
        removeThreat identityId attrs (if physical then 5 else 3) ThwartableScheme
        pure e
      _ -> SubOrbitalLeap <$> runMessage msg attrs
    _ -> SubOrbitalLeap <$> runMessage msg attrs
