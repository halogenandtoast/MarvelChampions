module Marvel.Event.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Text.Show qualified

data Event = forall a . IsEvent a => Event a

instance Show Event where
  show (Event a) = show a

instance ToJSON Event where
  toJSON (Event a) = toJSON a

instance Eq Event where
  (Event (a :: a)) == (Event (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeEventCard = forall a . IsEvent a => SomeEventCard (EventCard a)

liftEventCard :: (forall a . EventCard a -> b) -> SomeEventCard -> b
liftEventCard f (SomeEventCard a) = f a

someEventCardCode :: SomeEventCard -> CardCode
someEventCardCode = liftEventCard cbCardCode

instance HasModifiersFor Event where
  getModifiersFor source target (Event a) = getModifiersFor source target a

instance Entity Event where
  type EntityId Event = EventId
  type EntityAttrs Event = EventAttrs
  toId = toId . toAttrs
  toAttrs (Event a) = toAttrs a

instance RunMessage Event where
  runMessage msg (Event a) = Event <$> runMessage msg a

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, Entity a, EntityAttrs a ~ EventAttrs, EntityId a ~ EventId, HasModifiersFor a, RunMessage a) => IsEvent a

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

damageChoice :: EventAttrs -> Damage -> EnemyId -> Choice
damageChoice attrs dmg = \case
  EnemyVillainId vid -> TargetLabel
    (VillainTarget vid)
    [DamageEnemy (VillainTarget vid) (toSource attrs) dmg]
  EnemyMinionId vid -> TargetLabel
    (MinionTarget vid)
    [DamageEnemy (MinionTarget vid) (toSource attrs) dmg]

instance RunMessage EventAttrs where
  runMessage msg e = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      ResolvedEvent -> e <$ push (DiscardedCard $ toCard e)
      _ -> pure e
    _ -> pure e
