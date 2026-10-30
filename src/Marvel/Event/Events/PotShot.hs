module Marvel.Event.Events.PotShot where

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

potShot :: EventCard PotShot
potShot = event PotShot Cards.potShot

newtype PotShot = PotShot (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PotShot where
  runMessage msg e@(PotShot attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs ->
      e <$ chooseDamage you attrs (toDamage 4 $ FromPlayerAttack you) AttackableEnemy
    _ -> PotShot <$> runMessage msg attrs
