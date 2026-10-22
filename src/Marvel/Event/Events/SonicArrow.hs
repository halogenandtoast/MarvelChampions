module Marvel.Event.Events.SonicArrow (
  sonicArrow,
  SonicArrow (..),
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
import Marvel.Event.Arrow

sonicArrow :: EventCard SonicArrow
sonicArrow = event SonicArrow Cards.sonicArrow

newtype SonicArrow = SonicArrow (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Confuse an enemy and deal 3 damage to it (5 damage instead if it is already confused). The confused is read before it is confused again.
instance RunMessage SonicArrow where
  runMessage msg e@(SonicArrow attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne identityId
        [ targeting (enemyRef enemy) do
            already <- selectAny $ EnemyWithId enemy <> ConfusedEnemy
            dmg <- arrowDamage identityId (if already then 5 else 3)
            confuse enemy attrs
            dealDamage enemy attrs dmg
        | enemy <- enemies
        ]
      pure e
    _ -> SonicArrow <$> runMessage msg attrs
