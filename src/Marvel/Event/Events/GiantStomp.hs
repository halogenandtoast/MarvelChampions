module Marvel.Event.Events.GiantStomp (
  giantStomp,
  GiantStomp (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Damage

giantStomp :: EventCard GiantStomp
giantStomp = event GiantStomp Cards.giantStomp

newtype GiantStomp = GiantStomp (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 1 damage to each minion. Deal 8 damage to an enemy. The enemy is chosen once
-- the minions have taken their damage.
instance RunMessage GiantStomp where
  runMessage msg e@(GiantStomp attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      minions <- selectList AnyMinion
      dealDamageToEach (map EnemyMinionId minions) attrs (toDamage 1 $ FromPlayerAttack you)
      cardSpecific attrs "stomp" you
      pure e
    CardSpecific (isTarget attrs -> True) "stomp" (fromJSON -> Success you) -> do
      chooseDamage you attrs (toDamage 8 $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> GiantStomp <$> runMessage msg attrs
