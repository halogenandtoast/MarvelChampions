module Marvel.Event.Events.GroundStomp
  ( groundStomp
  , GroundStomp(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import qualified Marvel.Event.Cards as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target

groundStomp :: EventCard GroundStomp
groundStomp = event GroundStomp Cards.groundStomp

newtype GroundStomp = GroundStomp EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage GroundStomp where
  runMessage msg e@(GroundStomp attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AnyEnemy
        chooseOneAtATime identityId $ map (damageChoice 1 attrs) enemies
        pure e
      _ -> GroundStomp <$> runMessage msg attrs
    _ -> GroundStomp <$> runMessage msg attrs
