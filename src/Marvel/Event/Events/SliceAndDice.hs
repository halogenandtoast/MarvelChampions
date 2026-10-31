module Marvel.Event.Events.SliceAndDice (
  sliceAndDice,
  SliceAndDice (..),
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

sliceAndDice :: EventCard SliceAndDice
sliceAndDice = event SliceAndDice Cards.sliceAndDice

newtype SliceAndDice = SliceAndDice (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Two attacks in order, each dealing 3 damage: each its own attack, so a stun
-- prevents only the first, and guard applies to each.
instance RunMessage SliceAndDice where
  runMessage msg e@(SliceAndDice attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ _) | ident == eventId attrs -> do
      cardSpecific attrs "attack" (1 :: Natural)
      pure e
    CardSpecific (isTarget attrs -> True) "attack" (fromJSON -> Success (n :: Natural)) -> do
      let you = eventController attrs
      present <- identityMatches AnyIdentity you
      when present do
        stunned <- identityMatches StunnedIdentity you
        if stunned
          then removeStun you
          else chooseDamage you attrs (toDamage 3 $ FromPlayerAttack you) AttackableEnemy
        when (n < 2) $ cardSpecific attrs "attack" (n + 1)
      pure e
    _ -> SliceAndDice <$> runMessage msg attrs
