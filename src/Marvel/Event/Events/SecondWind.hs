module Marvel.Event.Events.SecondWind (
  secondWind,
  SecondWind (..),
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
import Marvel.Query
import Marvel.Ref
import Marvel.Resource

secondWind :: EventCard SecondWind
secondWind = event SecondWind Cards.secondWind

newtype SecondWind = SecondWind (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SecondWind where
  runMessage msg e@(SecondWind attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId payment _ -> do
        mental <- paidWith Mental payment
        let n = if mental then 5 else 4
        identities <- selectList AnyIdentity
        chooseOrRunOne identityId [targeting p $ heal p n | p <- identities]
        pure e
      _ -> SecondWind <$> runMessage msg attrs
    _ -> SecondWind <$> runMessage msg attrs
