module Marvel.Event.Events.SwingingWebKick
  ( SwingingWebKick
  , swingingWebKick
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Window

swingingWebKick :: EventCard SwingingWebKick
swingingWebKick = event SwingingWebKick Cards.swingingWebKick

newtype SwingingWebKick = SwingingWebKick EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage SwingingWebKick where
  runMessage msg e@(SwingingWebKick attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AttackableEnemy
        chooseOne identityId $ map (damageChoice attrs FromAttack 8) enemies
        pure e
      _ -> SwingingWebKick <$> runMessage msg attrs
    _ -> SwingingWebKick <$> runMessage msg attrs
