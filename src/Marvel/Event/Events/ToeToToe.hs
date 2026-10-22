module Marvel.Event.Events.ToeToToe (
  toeToToe,
  ToeToToe (..),
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

toeToToe :: EventCard ToeToToe
toeToToe = event ToeToToe Cards.toeToToe

newtype ToeToToe = ToeToToe (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Choose an enemy. That enemy attacks you. Deal 5 damage to that enemy."
instance RunMessage ToeToToe where
  runMessage msg e@(ToeToToe attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [ targeting (enemyRef enemy) do
              enemyAttacks enemy identityId
              dealDamage enemy attrs (toDamage 5 $ FromPlayerAttack identityId)
          | enemy <- enemies
          ]
        pure e
      _ -> ToeToToe <$> runMessage msg attrs
    _ -> ToeToToe <$> runMessage msg attrs
