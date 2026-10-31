module Marvel.Event.Events.BlastOfWind where

import Marvel.Prelude
import Data.Maybe (maybeToList)
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

blastOfWind :: EventCard BlastOfWind
blastOfWind = event BlastOfWind Cards.blastOfWind

newtype BlastOfWind = BlastOfWind (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BlastOfWind where
  runMessage msg e@(BlastOfWind attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      players <- selectList AnyIdentity
      chooseOne you [targeting player (cardSpecific attrs "blast" (you, player)) | player <- players]
      pure e
    CardSpecific (isTarget attrs -> True) "blast" (fromJSON -> Success (you :: IdentityId, player :: IdentityId)) -> do
      withTheVillainIfAny you \villain -> do
        minions <- selectList (MinionEngagedWith $ IdentityWithId player)
        dealDamageToEach (map EnemyVillainId (maybeToList villain) <> map EnemyMinionId minions) attrs (toDamage 3 FromAbility)
        resolveCurrentWeatherSpecial you
      pure e
    _ -> BlastOfWind <$> runMessage msg attrs
