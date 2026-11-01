module Marvel.Event.Events.ConcussiveBlast where

import Marvel.Prelude
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Query
import Marvel.Card
import Marvel.Damage
import Marvel.Matchers
import Marvel.Payment
import Marvel.Ability ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

concussiveBlast :: EventCard ConcussiveBlast
concussiveBlast = event ConcussiveBlast Cards.concussiveBlast

newtype ConcussiveBlast = ConcussiveBlast (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)



instance RunMessage ConcussiveBlast where
  runMessage msg a@(ConcussiveBlast attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      chooseDamage you attrs (toDamage 6 $ FromPlayerAttack you) AttackableEnemy
      when (any (cardMatch $ CardWithType ResourceType) $ paymentSpentCards payment) $ selectList (IdentityIs Heroes.bishopHero) >>= traverse_ ready
      pure a
    _ -> ConcussiveBlast <$> runMessage msg attrs
