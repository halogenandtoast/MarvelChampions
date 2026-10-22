module Marvel.Event.Events.ElectricArrow (
  electricArrow,
  ElectricArrow (..),
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

electricArrow :: EventCard ElectricArrow
electricArrow = event ElectricArrow Cards.electricArrow

newtype ElectricArrow = ElectricArrow (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Stun an enemy and deal 3 damage to it (5 damage instead if it is already stunned). The stunned is read before it is stunned again.
instance RunMessage ElectricArrow where
  runMessage msg e@(ElectricArrow attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne identityId
        [ targeting (enemyRef enemy) do
            already <- selectAny $ EnemyWithId enemy <> StunnedEnemy
            dmg <- arrowDamage identityId (if already then 5 else 3)
            stun enemy attrs
            dealDamage enemy attrs dmg
        | enemy <- enemies
        ]
      pure e
    _ -> ElectricArrow <$> runMessage msg attrs
