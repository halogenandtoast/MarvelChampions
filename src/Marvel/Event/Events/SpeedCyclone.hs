module Marvel.Event.Events.SpeedCyclone (
  speedCyclone,
  SpeedCyclone (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

speedCyclone :: EventCard SpeedCyclone
speedCyclone = event SpeedCyclone Cards.speedCyclone

newtype SpeedCyclone = SpeedCyclone (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Stun X enemies. X is the resources paid; that many different enemies
-- are chosen one at a time, every enemy when X is as many or more (owner ruling).
instance RunMessage SpeedCyclone where
  runMessage msg e@(SpeedCyclone attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      stunSome you (paymentX payment) []
      pure e
    CardSpecific (isTarget attrs -> True) "stun" (fromJSON -> Success (you, x, chosen)) -> do
      stunSome you x chosen
      pure e
    _ -> SpeedCyclone <$> runMessage msg attrs
   where
    stunSome :: (ReverseQueue m) => IdentityId -> Natural -> [EnemyId] -> m ()
    stunSome you x chosen = do
      enemies <- filter (`notElem` chosen) <$> selectList AnyEnemy
      if
        | x == 0 -> pure ()
        | fromIntegral x >= length enemies -> for_ enemies (`stun` attrs)
        | otherwise ->
            chooseOnePrompted you "$label.chooseEnemyToStun"
              [ targeting (enemyRef enemy) do
                  stun enemy attrs
                  cardSpecific attrs "stun" (you, x - 1, enemy : chosen)
              | enemy <- enemies
              ]
