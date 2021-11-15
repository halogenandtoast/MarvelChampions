{-# LANGUAGE UndecidableInstances #-}
module Marvel.Event.Events.Haymaker where

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

instance RunMessage Haymaker where
  runMessage msg a = case msg of
    EventMessage eid msg' | eid == toId a -> case msg' of
      PlayedEvent identityId _ -> do
        enemies <- selectList AnyEnemy
        pushAll
          [ Ask identityId $ ChooseOne $ map (damageChoice eid) enemies
          , IdentityMessage identityId $ DiscardCard (toCard $ toAttrs a)
          ]
        pure a
    _ -> pure a
