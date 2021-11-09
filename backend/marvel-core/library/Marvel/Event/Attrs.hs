module Marvel.Event.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
import Marvel.Source
import Marvel.Target

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

isTarget :: (Entity a, EntityAttrs a ~ EventAttrs) => a -> Target -> Bool
isTarget a = (== toTarget (toAttrs a))
