module Marvel.Event.Events.SupersonicPunch (
  supersonicPunch,
  SupersonicPunch (..),
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
import Marvel.Trait

supersonicPunch :: EventCard SupersonicPunch
supersonicPunch = event SupersonicPunch Cards.supersonicPunch

newtype SupersonicPunch = SupersonicPunch (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SupersonicPunch where
  runMessage msg e@(SupersonicPunch attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        aerial <- selectAny $ IdentityWithId identityId <> IdentityWithTrait Aerial
        let dmg = if aerial then 8 else 4
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage dmg $ FromPlayerAttack identityId) | enemy <- enemies]
        pure e
      _ -> SupersonicPunch <$> runMessage msg attrs
    _ -> SupersonicPunch <$> runMessage msg attrs
