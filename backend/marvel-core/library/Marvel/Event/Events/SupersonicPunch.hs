module Marvel.Event.Events.SupersonicPunch
  ( supersonicPunch
  , SupersonicPunch(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Source
import Marvel.Target
import Marvel.Trait

supersonicPunch :: EventCard SupersonicPunch
supersonicPunch = event SupersonicPunch Cards.supersonicPunch

newtype SupersonicPunch = SupersonicPunch (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage SupersonicPunch where
  runMessage msg e@(SupersonicPunch attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        aerial <-
          selectAny $ IdentityWithId identityId <> IdentityWithTrait Aerial
        let dmg = if aerial then 8 else 4
        enemies <- selectList AttackableEnemy
        chooseOne identityId $ map
          (damageChoice attrs (toDamage dmg $ FromPlayerAttack identityId))
          enemies
        pure e
      _ -> SupersonicPunch <$> runMessage msg attrs
    _ -> SupersonicPunch <$> runMessage msg attrs
