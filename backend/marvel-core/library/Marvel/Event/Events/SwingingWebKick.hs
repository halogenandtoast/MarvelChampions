{-# LANGUAGE UndecidableInstances #-}
module Marvel.Event.Events.SwingingWebKick where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import qualified Marvel.Event.Cards as Cards
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target

swingingWebKick :: EventCard SwingingWebKick
swingingWebKick = event SwingingWebKick Cards.swingingWebKick

newtype SwingingWebKick = SwingingWebKick EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

damageChoice :: EventId -> EnemyId -> Choice
damageChoice eid = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [Damage (VillainTarget vid) (EventSource eid) 8]

instance RunMessage SwingingWebKick where
  runMessage msg a = case msg of
    EventMessage eid msg' | eid == toId a -> case msg' of
      PlayedEvent identityId _ -> do
        enemies <- selectList AnyEnemy
        push $ Ask identityId $ ChooseOne $ map (damageChoice eid) enemies
        pure a
    _ -> pure a
