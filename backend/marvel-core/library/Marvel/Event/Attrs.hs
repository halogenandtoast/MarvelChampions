module Marvel.Event.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Id
import Marvel.Card.PlayerCard
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Queue
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

toCard :: EventAttrs -> PlayerCard
toCard a = PlayerCard
  { pcCardId = CardId . unEventId $ toId a
  , pcCardDef = eventCardDef a
  , pcOwner = Just $ eventController a
  , pcController = Just $ eventController a
  }

instance RunMessage EventAttrs where
  runMessage msg e = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      ResolvedEvent ->
        e <$ push (IdentityMessage (eventController e) $ DiscardCard (toCard e))
      _ -> pure e
    _ -> pure e
