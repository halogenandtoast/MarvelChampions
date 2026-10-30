module Marvel.Event.Events.Pitchback where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

pitchback :: EventCard Pitchback
pitchback = event Pitchback Cards.pitchback

newtype Pitchback = Pitchback (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Pitchback where
  runMessage msg e@(Pitchback attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      chooseDamage you attrs (toDamage 4 $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> Pitchback <$> runMessage msg attrs
