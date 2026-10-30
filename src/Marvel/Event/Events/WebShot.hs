module Marvel.Event.Events.WebShot where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Damage
import Marvel.Matchers
import Marvel.Payment
import Marvel.Resource

webShot :: EventCard WebShot
webShot = event WebShot Cards.webShot

newtype WebShot = WebShot (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage WebShot where
  runMessage msg e@(WebShot attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      energy <- paidWith Energy payment
      chooseDamage you attrs (toDamage 4 $ FromPlayerAttack you) AttackableEnemy
      when energy $ cardSpecific (IdentityRef you) "venomBlast" ()
      pure e
    _ -> WebShot <$> runMessage msg attrs
