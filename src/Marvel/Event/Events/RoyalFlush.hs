module Marvel.Event.Events.RoyalFlush where

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

royalFlush :: EventCard RoyalFlush
royalFlush = event RoyalFlush Cards.royalFlush

newtype RoyalFlush = RoyalFlush (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RoyalFlush where
  runMessage msg e@(RoyalFlush attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      placeCounters you 1
      cardSpecific attrs "royalFlushDamage" (you, 3 :: Natural)
      pure e
    CardSpecific (isTarget attrs -> True) "royalFlushDamage" (fromJSON -> Success (you, remaining :: Natural)) -> do
      when (remaining > 0) do
        chooseDamage you attrs (toDamage 0 $ FromPlayerAttack you) AttackableEnemy
        cardSpecific attrs "royalFlushDamage" (you, remaining - 1)
      pure e
    _ -> RoyalFlush <$> runMessage msg attrs
