module Marvel.Event.Events.Shieldmaiden (shieldmaiden, Shieldmaiden (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

shieldmaiden :: EventCard Shieldmaiden
shieldmaiden = event Shieldmaiden Cards.shieldmaiden

newtype Shieldmaiden = Shieldmaiden (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Shieldmaiden where
  runMessage msg e@(Shieldmaiden attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.EnemyAttack enemy _))) | eid == eventId attrs -> do
      declareHeroDefenseWithoutExhausting you enemy 2
      pure e
    _ -> Shieldmaiden <$> runMessage msg attrs
