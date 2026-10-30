module Marvel.Event.Events.FlyOver where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Query
import Marvel.Window qualified as W

flyOver :: EventCard FlyOver
flyOver = event FlyOver Cards.flyOver

newtype FlyOver = FlyOver (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FlyOver where
  runMessage msg e@(FlyOver attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      schemes <- selectList ThwartableScheme
      chooseOne you [targeting (SchemeRef scheme) do
        thwartScheme scheme attrs 3
        cardSpecific attrs "progress" (1 :: Natural)
        | scheme <- schemes]
      pure e
    CheckWindows windows -> do
      for_ [() | W.Window W.After (W.ThreatRemovedBy source _ before removed) <- windows
               , source == toSource attrs, before > 0, removed == before] $ \_ ->
        insteadOf pending $ cardSpecific attrs "progress" (2 :: Natural)
      FlyOver <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "progress" (fromJSON -> Success (n :: Natural)) ->
      e <$ placeCounters (eventController attrs) n
    _ -> FlyOver <$> runMessage msg attrs

   where
    pending = \case
      CardSpecific (isTarget attrs -> True) "progress" _ -> True
      _ -> False
