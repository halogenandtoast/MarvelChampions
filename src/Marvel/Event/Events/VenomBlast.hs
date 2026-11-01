module Marvel.Event.Events.VenomBlast (
  venomBlast,
  VenomBlast (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref

venomBlast :: EventCard VenomBlast
venomBlast = event VenomBlast Cards.venomBlast

newtype VenomBlast = VenomBlast (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 5 damage to an enemy.
instance RunMessage VenomBlast where
  runMessage msg e@(VenomBlast attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      chooseDamage identityId attrs (toDamage 5 $ FromPlayerAttack identityId) AttackableEnemy
      pure e
    _ -> VenomBlast <$> runMessage msg attrs
