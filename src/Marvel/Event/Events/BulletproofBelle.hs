module Marvel.Event.Events.BulletproofBelle where

import Marvel.Id
import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Window qualified as W

bulletproofBelle :: EventCard BulletproofBelle
bulletproofBelle = event BulletproofBelle Cards.bulletproofBelle

newtype BulletproofBelle = BulletproofBelle (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BulletproofBelle where
  runMessage msg e@(BulletproofBelle attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.EnemyAttack enemy _))) | eid == eventId attrs -> do
      host <- touchedHost you
      let attacker = case enemy of
            EnemyVillainId v -> VillainCharacter v
            EnemyMinionId m -> MinionCharacter m
      when (host == Just attacker) do
        preventAttackDamage enemy
        becomeTough you
      pure e
    _ -> BulletproofBelle <$> runMessage msg attrs
