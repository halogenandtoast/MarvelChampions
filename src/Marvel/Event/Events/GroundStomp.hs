module Marvel.Event.Events.GroundStomp (
  groundStomp,
  GroundStomp (..),
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
import Marvel.Ref

groundStomp :: EventCard GroundStomp
groundStomp = event GroundStomp Cards.groundStomp

newtype GroundStomp = GroundStomp (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GroundStomp where
  runMessage msg e@(GroundStomp attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent _ _ _ -> do
        -- not an attack: no attack window, no retaliate
        damageAllEnemies attrs (toDamage 1 FromAbility) AnyEnemy
        pure e
      _ -> GroundStomp <$> runMessage msg attrs
    _ -> GroundStomp <$> runMessage msg attrs
