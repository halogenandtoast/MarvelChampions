module Marvel.Event.Events.GroundStomp
  ( groundStomp
  , GroundStomp(..)
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
        chooseOneAtATime identityId
          $ map (damageChoice attrs FromAttack 1) enemies
        pure e
      _ -> GroundStomp <$> runMessage msg attrs
    _ -> GroundStomp <$> runMessage msg attrs
