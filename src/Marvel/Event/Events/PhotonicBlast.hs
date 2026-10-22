module Marvel.Event.Events.PhotonicBlast (
  photonicBlast,
  PhotonicBlast (..),
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
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Resource

photonicBlast :: EventCard PhotonicBlast
photonicBlast = event PhotonicBlast Cards.photonicBlast

newtype PhotonicBlast = PhotonicBlast (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PhotonicBlast where
  runMessage msg e@(PhotonicBlast attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId payments _ -> do
        usedEnergy <- paidWith Energy payments
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage 5 $ FromPlayerAttack identityId) | enemy <- enemies]
        when usedEnergy $ drawCards identityId 1
        pure e
      _ -> PhotonicBlast <$> runMessage msg attrs
    _ -> PhotonicBlast <$> runMessage msg attrs
