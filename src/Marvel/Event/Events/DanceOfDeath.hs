module Marvel.Event.Events.DanceOfDeath (
  danceOfDeath,
  DanceOfDeath (..),
) where

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

danceOfDeath :: EventCard DanceOfDeath
danceOfDeath = event DanceOfDeath Cards.danceOfDeath

newtype DanceOfDeath = DanceOfDeath (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Three attacks in order, of 1, 2 and 3 damage: each its own attack, so a stun
-- prevents only the first, and guard applies to each.
instance RunMessage DanceOfDeath where
  runMessage msg e@(DanceOfDeath attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ _) | ident == eventId attrs -> do
      cardSpecific attrs "attack" (1 :: Natural)
      pure e
    CardSpecific (isTarget attrs -> True) "attack" (fromJSON -> Success (n :: Natural)) -> do
      let you = eventController attrs
      stunned <- identityMatches StunnedIdentity you
      if stunned
        then removeStun you
        else chooseDamage you attrs (toDamage n $ FromPlayerAttack you) AttackableEnemy
      when (n < 3) $ cardSpecific attrs "attack" (n + 1)
      pure e
    _ -> DanceOfDeath <$> runMessage msg attrs
