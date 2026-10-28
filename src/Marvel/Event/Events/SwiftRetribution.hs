module Marvel.Event.Events.SwiftRetribution (
  swiftRetribution,
  SwiftRetribution (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Id
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

swiftRetribution :: EventCard SwiftRetribution
swiftRetribution = event SwiftRetribution Cards.swiftRetribution

newtype SwiftRetribution = SwiftRetribution (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): the villain schemes, then 4 damage to the villain.
instance RunMessage SwiftRetribution where
  runMessage msg e@(SwiftRetribution attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      -- the one villain, chosen once, schemes and is attacked
      withTheVillain you \v -> do
        villainSchemes v
        cardSpecific attrs "attack" (you, v)
      pure e
    -- its own step: the villain as his scheme left him
    CardSpecific (isTarget attrs -> True) "attack" (fromJSON -> Success (you, v :: VillainId)) -> do
      chooseDamage you attrs (toDamage 4 $ FromPlayerAttack you) (EnemyWithId (EnemyVillainId v) <> AttackableEnemy)
      pure e
    _ -> SwiftRetribution <$> runMessage msg attrs
