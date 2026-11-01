module Marvel.Event.Events.PressTheAdvantage (
  pressTheAdvantage,
  PressTheAdvantage (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

pressTheAdvantage :: EventCard PressTheAdvantage
pressTheAdvantage = event PressTheAdvantage Cards.pressTheAdvantage

newtype PressTheAdvantage = PressTheAdvantage (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 2 damage to an enemy. If that enemy is stunned or confused, draw 1 card.
instance RunMessage PressTheAdvantage where
  runMessage msg e@(PressTheAdvantage attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne identityId
        [ targeting (enemyRef enemy) do
            stunned <- selectAny $ EnemyWithId enemy <> StunnedEnemy
            confused <- selectAny $ EnemyWithId enemy <> ConfusedEnemy
            dealDamage enemy attrs (toDamage 2 $ FromPlayerAttack identityId)
            when (stunned || confused) $ drawCards identityId 1
        | enemy <- enemies
        ]
      pure e
    _ -> PressTheAdvantage <$> runMessage msg attrs
