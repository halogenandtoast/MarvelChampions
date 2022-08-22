module Marvel.Event.Events.SwingingWebKick
  ( SwingingWebKick
  , swingingWebKick
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target

swingingWebKick :: EventCard SwingingWebKick
swingingWebKick = event SwingingWebKick Cards.swingingWebKick

newtype SwingingWebKick = SwingingWebKick EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage SwingingWebKick where
  runMessage msg e@(SwingingWebKick attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AttackableEnemy
        chooseOne identityId $ map
          (damageChoice attrs (toDamage 8 $ FromPlayerAttack identityId))
          enemies
        pure e
      _ -> SwingingWebKick <$> runMessage msg attrs
    _ -> SwingingWebKick <$> runMessage msg attrs
