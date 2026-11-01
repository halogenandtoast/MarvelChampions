module Marvel.Event.Events.BombsAway where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
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
import Marvel.Payment
import Marvel.Damage
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

bombsAway :: EventCard BombsAway
bombsAway = event BombsAway Cards.bombsAway
newtype BombsAway = BombsAway (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent BombsAway where
  eventTargets _ = Just . map toTarget <$> getPlayers

instance RunMessage BombsAway where
  runMessage msg e@(BombsAway attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      for_ (paymentTarget payment) $ \case
        IdentityRef player -> cardSpecific attrs "bomb" (you, player)
        _ -> pure ()
      pure e
    CardSpecific (isTarget attrs -> True) "bomb" (fromJSON -> Success (you, player :: IdentityId)) -> do
      minions <- selectList $ MinionEngagedWith $ IdentityWithId player
      withTheVillain you $ \villain -> dealDamageToEach (EnemyVillainId villain : map EnemyMinionId minions) attrs (toDamage 3 FromAbility)
      pure e
    _ -> BombsAway <$> runMessage msg attrs
