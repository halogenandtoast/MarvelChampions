module Marvel.Event.Events.DanceOfDeath (
  danceOfDeath,
  DanceOfDeath (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

danceOfDeath :: EventCard DanceOfDeath
danceOfDeath = event DanceOfDeath Cards.danceOfDeath

newtype DanceOfDeath = DanceOfDeath (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Three attacks in order, of 1, 2 and 3 damage: each its own attack, so a stun
-- prevents only the first, and guard applies to each.
instance RunMessage DanceOfDeath where
  runMessage msg e@(DanceOfDeath attrs) = case msg of
    EventMessage ident (PlayedEvent _ _ _) | ident == eventId attrs -> do
      push $ cardSpecific attrs "attack" (1 :: Natural)
      pure e
    CardSpecific (isTarget attrs -> True) "attack" (fromJSON -> Success (n :: Natural)) -> do
      let
        you = eventController attrs
        next = [cardSpecific attrs "attack" (n + 1) | n < 3]
      stunned <- identityMatches StunnedIdentity you
      attack <-
        if stunned
          then pure [IdentityMessage you IdentityRemoveStunned]
          else choiceMessages you $ ChooseDamage (toSource attrs) (toDamage n $ FromPlayerAttack you) AttackableEnemy
      pushAll $ attack <> next
      pure e
    _ -> DanceOfDeath <$> runMessage msg attrs
