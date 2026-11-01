module Marvel.Event.Events.PiercingStrike (
  piercingStrike,
  PiercingStrike (..),
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

piercingStrike :: EventCard PiercingStrike
piercingStrike = event PiercingStrike Cards.piercingStrike

newtype PiercingStrike = PiercingStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 3 damage to an enemy. This attack gains piercing.
instance RunMessage PiercingStrike where
  runMessage msg e@(PiercingStrike attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      chooseDamage identityId attrs (withPiercing $ toDamage 3 $ FromPlayerAttack identityId) AttackableEnemy
      pure e
    _ -> PiercingStrike <$> runMessage msg attrs
