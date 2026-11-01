module Marvel.Event.Events.EnergyConversion where

import Marvel.Prelude
import Marvel.Card
import Marvel.Discard
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Projection
import Marvel.Window qualified as W
import Marvel.Ability ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

energyConversion :: EventCard EnergyConversion
energyConversion = event EnergyConversion Cards.energyConversion

newtype EnergyConversion = EnergyConversion (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)



instance RunMessage EnergyConversion where
  runMessage msg a@(EnergyConversion attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ window) | eid == eventId attrs -> do
      cards <- filter (cardMatch $ CardWithType ResourceType) <$> projectMap PlayerIdentityDiscard unDiscard you
      unless (null cards) $ shuffleIntoDeck you cards
      for_ window $ \case
        W.EnemyAttack enemy _ -> capEnemyAttackDamage enemy you 3
        W.EnemyAttackAlly enemy _ _ -> capEnemyAttackDamage enemy you 3
        _ -> pure ()
      pure a
    _ -> EnergyConversion <$> runMessage msg attrs
