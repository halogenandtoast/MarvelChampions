module Marvel.Event.Events.GroundStomp (
  groundStomp,
  GroundStomp (..),
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
import Marvel.Ref

groundStomp :: EventCard GroundStomp
groundStomp = event GroundStomp Cards.groundStomp

newtype GroundStomp = GroundStomp (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GroundStomp where
  runMessage msg e@(GroundStomp attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AnyEnemy
        chooseOneAtATime identityId $
          map
            (damageChoice attrs (toDamage 1 $ FromPlayerAttack identityId))
            enemies
        pure e
      _ -> GroundStomp <$> runMessage msg attrs
    _ -> GroundStomp <$> runMessage msg attrs
