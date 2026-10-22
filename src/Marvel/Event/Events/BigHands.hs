module Marvel.Event.Events.BigHands (
  bigHands,
  BigHands (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

bigHands :: EventCard BigHands
bigHands = event BigHands Cards.bigHands

newtype BigHands = BigHands (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BigHands where
  runMessage msg e@(BigHands attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage 4 $ FromPlayerAttack identityId) | enemy <- enemies]
        pure e
      _ -> BigHands <$> runMessage msg attrs
    _ -> BigHands <$> runMessage msg attrs
