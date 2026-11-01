module Marvel.Event.Events.AerialIntervention where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

aerialIntervention :: EventCard AerialIntervention
aerialIntervention = event AerialIntervention Cards.aerialIntervention
newtype AerialIntervention = AerialIntervention (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AerialIntervention where
  runMessage msg e@(AerialIntervention attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ window) | eid == eventId attrs -> do
      case window of
        Just (W.IdentityTakeDamage you damage) -> do
          when (piercesTough damage) $ removeAllTough you
          preventDamage you 3
        Just (W.IdentityTakeDamageFrom you _ damage) -> do
          when (piercesTough damage) $ removeAllTough you
          preventDamage you 3
        Just (W.AllyTakeDamage aid damage) -> do
          when (piercesTough damage) $ removeAllTough aid
          preventAllyDamage aid 3
        Just (W.AllyTakeDamageFrom aid _ damage) -> do
          when (piercesTough damage) $ removeAllTough aid
          preventAllyDamage aid 3
        Just (W.EnemyWouldBeDamaged enemy _ _) -> preventEnemyDamage enemy 3
        _ -> pure ()
      pure e
    _ -> AerialIntervention <$> runMessage msg attrs
