module Marvel.Event.Events.TheBestDefense (theBestDefense, TheBestDefense (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

theBestDefense :: EventCard TheBestDefense
theBestDefense = event TheBestDefense Cards.theBestDefense

newtype TheBestDefense = TheBestDefense (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TheBestDefense where
  runMessage msg e@(TheBestDefense attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.HeroDefends _ _))) | eid == eventId attrs -> do
      useAttackForDefense you
      pure e
    _ -> TheBestDefense <$> runMessage msg attrs
