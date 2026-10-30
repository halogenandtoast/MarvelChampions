module Marvel.Event.Events.WebBinding where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Id (EnemyId (..))
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

webBinding :: EventCard WebBinding
webBinding = event WebBinding Cards.webBinding

newtype WebBinding = WebBinding (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage WebBinding where
  runMessage msg e@(WebBinding attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ (Just (W.EnemyActivation enemy _))) | eid == eventId attrs -> do
      cancelEnemyActivation enemy
      case enemy of
        EnemyMinionId mid -> dealDamage mid attrs (toDamage 4 FromAbility)
        EnemyVillainId _ -> pure ()
      pure e
    _ -> WebBinding <$> runMessage msg attrs
