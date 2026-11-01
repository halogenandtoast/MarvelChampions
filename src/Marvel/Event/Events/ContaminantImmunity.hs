module Marvel.Event.Events.ContaminantImmunity (
  contaminantImmunity,
  ContaminantImmunity (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref

contaminantImmunity :: EventCard ContaminantImmunity
contaminantImmunity = event ContaminantImmunity Cards.contaminantImmunity

newtype ContaminantImmunity = ContaminantImmunity (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Heal 3 damage from Spider-Woman and give her a tough status card.
instance RunMessage ContaminantImmunity where
  runMessage msg e@(ContaminantImmunity attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      heal (IdentityCharacter identityId) 3
      becomeTough identityId
      pure e
    _ -> ContaminantImmunity <$> runMessage msg attrs
