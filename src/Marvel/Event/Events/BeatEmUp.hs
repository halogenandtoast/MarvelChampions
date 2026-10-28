module Marvel.Event.Events.BeatEmUp (
  beatEmUp,
  BeatEmUp (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
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

beatEmUp :: EventCard BeatEmUp
beatEmUp = event BeatEmUp Cards.beatEmUp

newtype BeatEmUp = BeatEmUp (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Hero Action: Deal 1 damage to the villain and each minion engaged with you." Not an
-- attack: damage from an ability, all of it at once.
instance RunMessage BeatEmUp where
  runMessage msg e@(BeatEmUp attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      minions <- selectList $ MinionEngagedWith (IdentityWithId you)
      withTheVillainIfAny you \villain ->
        dealDamageToEach (maybe [] (pure . EnemyVillainId) villain <> map EnemyMinionId minions) attrs (toDamage 1 FromAbility)
      pure e
    _ -> BeatEmUp <$> runMessage msg attrs
