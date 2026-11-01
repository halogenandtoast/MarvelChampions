module Marvel.Event.Events.RazorDive where

import Marvel.Prelude
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Card.Side
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

razorDive :: EventCard RazorDive
razorDive = event RazorDive Cards.razorDive

newtype RazorDive = RazorDive (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RazorDive where
  runMessage msg e@(RazorDive attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      archangel <- identityMatches (IdentityWithTitle "Archangel") you
      let damage = toDamage 6 (FromPlayerAttack you)
      chooseDamage you attrs (if archangel then withPiercing $ withOverkill damage else damage) AttackableEnemy
      pure e
    _ -> RazorDive <$> runMessage msg attrs
