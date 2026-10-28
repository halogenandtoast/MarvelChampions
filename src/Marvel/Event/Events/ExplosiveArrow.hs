module Marvel.Event.Events.ExplosiveArrow (
  explosiveArrow,
  ExplosiveArrow (..),
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
import Marvel.Game.Source

explosiveArrow :: EventCard ExplosiveArrow
explosiveArrow = event ExplosiveArrow Cards.explosiveArrow

newtype ExplosiveArrow = ExplosiveArrow (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Choose a player: deal 3 damage to the villain and each minion engaged with that player.
instance RunMessage ExplosiveArrow where
  runMessage msg e@(ExplosiveArrow attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      players <- getPlayers
      withTheVillainIfAny identityId \villain -> chooseOne identityId
        [ targeting p do
            minions <- selectList $ MinionEngagedWith (IdentityWithId p)
            dealDamageToEach (maybe [] (pure . EnemyVillainId) villain <> map EnemyMinionId minions) attrs (toDamage 3 FromAbility)
        | p <- players
        ]
      pure e
    _ -> ExplosiveArrow <$> runMessage msg attrs
