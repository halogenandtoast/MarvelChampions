module Marvel.Event.Events.ChargedCard where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

chargedCard :: EventCard ChargedCard
chargedCard = event ChargedCard Cards.chargedCard

newtype ChargedCard = ChargedCard (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ChargedCard where
  runMessage msg e@(ChargedCard attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      n <- sum . map (\case ChargedWithCounters k -> k; _ -> 0) <$> getModifiers attrs
      let dmg = (if n >= 3 then withOverkill else id)
              $ (if n >= 2 then withPiercing else id)
              $ (if n >= 1 then withRanged else id)
              $ toDamage 4 (FromPlayerAttack you)
      chooseDamage you attrs dmg AttackableEnemy
      pure e
    _ -> ChargedCard <$> runMessage msg attrs
