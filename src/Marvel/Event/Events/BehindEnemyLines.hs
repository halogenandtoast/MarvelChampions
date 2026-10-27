module Marvel.Event.Events.BehindEnemyLines (
  behindEnemyLines,
  BehindEnemyLines (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Resource

behindEnemyLines :: EventCard BehindEnemyLines
behindEnemyLines = event BehindEnemyLines Cards.behindEnemyLines

newtype BehindEnemyLines = BehindEnemyLines (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove 3 threat from a scheme; paid for using only [mental] resources, confuse an
-- enemy.
instance RunMessage BehindEnemyLines where
  runMessage msg e@(BehindEnemyLines attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      mental <- paidUsingOnly Mental payment
      removeThreat you attrs 3 ThwartableScheme
      when mental do
        enemies <- selectList AnyEnemy
        chooseOne you [targeting (enemyRef enemy) (confuse enemy attrs) | enemy <- enemies]
      pure e
    _ -> BehindEnemyLines <$> runMessage msg attrs
