module Marvel.Event.Attrs where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window qualified as W

class IsEvent a

type EventCard a = CardBuilder (IdentityId, EventId) a

data EventAttrs = EventAttrs
  { eventId :: EventId
  , eventCardDef :: CardDef
  , eventController :: IdentityId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardCode EventAttrs where
  toCardCode = toCardCode . eventCardDef

event :: (EventAttrs -> a) -> CardDef -> CardBuilder (IdentityId, EventId) a
event f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(ident, aid) -> f $ EventAttrs
    { eventId = aid
    , eventCardDef = cardDef
    , eventController = ident
    }
  }

instance Entity EventAttrs where
  type EntityId EventAttrs = EventId
  type EntityAttrs EventAttrs = EventAttrs
  toId = eventId
  toAttrs = id

instance IsSource EventAttrs where
  toSource = EventSource . toId

instance IsTarget EventAttrs where
  toTarget = EventTarget . toId

instance IsCard EventAttrs where
  toCard a = PlayerCard $ MkPlayerCard
    { pcCardId = CardId . unEventId $ toId a
    , pcCardDef = eventCardDef a
    , pcOwner = Just $ eventController a
    , pcController = Just $ eventController a
    }

damageChoice :: EventAttrs -> W.DamageSource -> Natural -> EnemyId -> Choice
damageChoice attrs damageSource dmg = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [DamageEnemy (VillainTarget vid) (toSource attrs) damageSource dmg]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [DamageEnemy (MinionTarget vid) (toSource attrs) damageSource dmg]

instance RunMessage EventAttrs where
  runMessage msg e = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      ResolvedEvent ->
        e <$ push (DiscardedCard $ toCard e)
      _ -> pure e
    _ -> pure e
