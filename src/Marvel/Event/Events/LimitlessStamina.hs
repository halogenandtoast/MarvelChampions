module Marvel.Event.Events.LimitlessStamina where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

limitlessStamina :: EventCard LimitlessStamina
limitlessStamina = event LimitlessStamina Cards.limitlessStamina

newtype LimitlessStamina = LimitlessStamina (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage LimitlessStamina where
  runMessage msg e@(LimitlessStamina attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> e <$ ready you
    _ -> LimitlessStamina <$> runMessage msg attrs
