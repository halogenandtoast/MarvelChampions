module Marvel.Event.Events.SwingingWebKick
  ( SwingingWebKick
  , swingingWebKick
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
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
    [DamageEnemy (VillainTarget vid) (EventSource eid) 8]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [DamageEnemy (MinionTarget vid) (EventSource eid) 8]

instance RunMessage SwingingWebKick where
  runMessage msg e@(SwingingWebKick attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AnyEnemy
        pushAll
          [ Ask identityId $ ChooseOne $ map (damageChoice eid) enemies
          , IdentityMessage identityId $ DiscardCard (toCard attrs)
          ]
        pure e
      _ -> SwingingWebKick <$> runMessage msg attrs
    _ -> SwingingWebKick <$> runMessage msg attrs
