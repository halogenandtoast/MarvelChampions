module Marvel.Event.Types where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Id hiding (EventId)
import Marvel.Id as X (EventId)
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

data Event = forall a. (IsEvent a) => Event a

instance Show Event where
  show (Event a) = show a

instance ToJSON Event where
  toJSON (Event a) = toJSON a

instance Eq Event where
  Event (a :: a) == Event (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeEventCard = forall a. (IsEvent a) => SomeEventCard (EventCard a)

liftEventCard :: (forall a. EventCard a -> b) -> SomeEventCard -> b
liftEventCard f (SomeEventCard a) = f a

someEventCardCode :: SomeEventCard -> CardCode
someEventCardCode = liftEventCard cbCardCode

instance HasModifiersFor Event where
  getModifiersFor source target (Event a) = getModifiersFor source target a

instance Entity Event where
  type Id Event = EventId
  data Attrs Event = EventAttrs
    { eventId :: EventId
    , eventCardDef :: CardDef
    , eventController :: IdentityId
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Event :: Type -> Type where
    EventId :: Field Event EventId
    EventCardDef :: Field Event CardDef
    EventController :: Field Event IdentityId
  field fld e =
    let EventAttrs {..} = toAttrs e
     in case fld of
          EventId -> eventId
          EventCardDef -> eventCardDef
          EventController -> eventController
  toId = eventId . toAttrs
  toAttrs (Event a) = toEventAttrs a

instance RunMessage Event where
  runMessage msg (Event a) = Event <$> runMessage msg a

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasModifiersFor a, RunMessage a) => IsEvent a where
  toEventAttrs :: a -> Attrs Event
  default toEventAttrs :: (Coercible a (Attrs Event)) => a -> Attrs Event
  toEventAttrs = coerce

type EventCard a = CardBuilder (IdentityId, EventId) a

instance HasCardCode (Attrs Event) where
  toCardCode = toCardCode . eventCardDef

event :: (Attrs Event -> a) -> CardDef -> CardBuilder (IdentityId, EventId) a
event f cardDef =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \(ident, aid) ->
        f $
          EventAttrs
            { eventId = aid
            , eventCardDef = cardDef
            , eventController = ident
            }
    }

instance IsRef (Attrs Event) where
  toRef = EventRef . eventId

instance IsCard (Attrs Event) where
  toCard a =
    PlayerCard $
      MkPlayerCard
        { pcCardId = CardId . unEventId $ eventId a
        , pcCardDef = eventCardDef a
        , pcOwner = Just $ eventController a
        , pcController = Just $ eventController a
        }

damageChoice :: Attrs Event -> Damage -> EnemyId -> Choice
damageChoice attrs dmg = \case
  EnemyVillainId vid ->
    TargetLabel
      (toRef vid)
      [DamageEnemy (toRef vid) (toRef attrs) dmg]
  EnemyMinionId vid ->
    TargetLabel
      (toRef vid)
      [DamageEnemy (toRef vid) (toRef attrs) dmg]

instance RunMessage (Attrs Event) where
  runMessage msg e = case msg of
    EventMessage ident msg' | ident == eventId e -> case msg' of
      ResolvedEvent -> e <$ push (DiscardedCard $ toCard e)
      _ -> pure e
    _ -> pure e
