module Marvel.Event.Events.LightningBolt where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

lightningBolt :: EventCard LightningBolt
lightningBolt = event LightningBolt Cards.lightningBolt

newtype LightningBolt = LightningBolt (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage LightningBolt where
  runMessage msg e@(LightningBolt attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      chooseDamage you attrs (toDamage 8 $ FromPlayerAttack you) AttackableEnemy
      cardSpecific attrs "weather" you
      pure e
    CardSpecific (isTarget attrs -> True) "weather" (fromJSON -> Success (you :: IdentityId)) -> do
      weather <- selectList (SupportNamed "Thunderstorm")
      for_ weather (resolveWeatherSpecial you)
      pure e
    _ -> LightningBolt <$> runMessage msg attrs
