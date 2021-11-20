module Marvel.Event.Events.Haymaker
  ( Haymaker
  , haymaker
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

haymaker :: EventCard Haymaker
haymaker = event Haymaker Cards.haymaker

newtype Haymaker = Haymaker EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

damageChoice :: EventId -> EnemyId -> Choice
damageChoice eid = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [DamageEnemy (VillainTarget vid) (EventSource eid) 3]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [DamageEnemy (MinionTarget vid) (EventSource eid) 3]

instance RunMessage Haymaker where
  runMessage msg e@(Haymaker attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AnyEnemy
        pushAll
          [ Ask identityId $ ChooseOne $ map (damageChoice eid) enemies
          , IdentityMessage identityId $ DiscardCard (toCard attrs)
          ]
        pure e
      _ -> Haymaker <$> runMessage msg attrs
    _ -> Haymaker <$> runMessage msg attrs
