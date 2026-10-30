module Marvel.Event.Types where

import Data.List (nub)

import Marvel.Prelude
import Marvel.Payment.Types
import Data.Aeson.KeyMap qualified as KeyMap

import Data.Typeable
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Identity.Types
import Marvel.Projection
import Marvel.Id hiding (EventId)
import Marvel.Id as X (EventId)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Queue
import Marvel.Ref
import Marvel.Ability.Types qualified as Ability
import Marvel.Window qualified as W

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
someEventCardCode = liftEventCard toCardCode

instance HasModifiersFor Event where
  getModifiersFor source target (Event a) = getModifiersFor source target a

instance Entity Event where
  type Id Event = EventId
  data Attrs Event = EventAttrs
    { eventId :: EventId
    , eventCardDef :: CardDef
    , eventController :: IdentityId
    , eventPaidWith :: Payment
    , eventThwarted :: Bool
    , -- | whom a labeled attack attacked, for the windows after it
      eventAttacked :: [(IdentityId, EnemyId, Bool)]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)
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

instance FromJSON (Attrs Event) where
  parseJSON = withObject "EventAttrs" $ \o -> genericParseJSON defaultOptions $ Object $
    KeyMap.insertWith (\_ existing -> existing) "eventThwarted" (Bool False) $
    KeyMap.insertWith (\_ existing -> existing) "eventPaidWith" (toJSON NoPayment) o

instance RunMessage Event where
  runMessage msg (Event a) = Event <$> runMessage msg a

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasModifiersFor a, RunMessage a) => IsEvent a where
  toEventAttrs :: a -> Attrs Event
  default toEventAttrs :: (Coercible a (Attrs Event)) => a -> Attrs Event
  toEventAttrs = coerce

  -- | The labels of the action being used. Most cards have one printed ability;
  -- Solar Beam selects a different labeled ability according to its mass form.
  eventAbilitySubTypes :: (HasGame m) => a -> m [Ability.AbilitySubType]
  eventAbilitySubTypes = pure . cdAbilitySubTypes . eventCardDef . toEventAttrs

  -- | What it is played on, chosen before its cost is paid. Nothing for an event
  -- played on nothing.
  eventTargets :: (HasGame m, HasQueue m, MonadThrow m, Projection m PlayerIdentity) => a -> m (Maybe [Target])
  eventTargets _ = pure Nothing

  -- | Validate the complete ordered characters paying exhaust-as-cost parts.
  -- Choices and Alliance contributor plans may only leave a valid complete set.
  eventExhaustionCostValid :: (HasGame m) => a -> [CharacterId] -> m Bool
  eventExhaustionCostValid _ _ = pure True

type EventCard a = CardBuilder (IdentityId, EventId) a

instance HasCardCode (Attrs Event) where
  toCardCode = toCardCode . eventCardDef

event :: (Attrs Event -> a) -> CardDef -> CardBuilder (IdentityId, EventId) a
event f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef (ident, aid) ->
        f $
          EventAttrs
            { eventId = aid
            , eventCardDef = cardDef
            , eventController = ident
            , eventPaidWith = NoPayment
            , eventThwarted = False
            , eventAttacked = []
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

instance RunMessage (Attrs Event) where
  runMessage msg e = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId e -> case msg' of
      EventPaidWith p -> pure e {eventPaidWith = p}
      EventThwarted -> pure e {eventThwarted = True}
      -- what answers a labeled attack follows the whole of it
      ResolvedEvent -> do
        when (eventThwarted e) $ checkWindows [W.Window W.After $ W.HeroThwartComplete (eventController e)]
        for_ (eventAttacked e) \(attacker, enemy, ranged) -> attackResolved attacker enemy ranged
        for_ (nub [attacker | (attacker, _, _) <- eventAttacked e]) \attacker ->
          checkWindows [W.Window W.After $ W.PlayerAttackResolved (toSource e) attacker (nub [enemy | (who, enemy, _) <- eventAttacked e, who == attacker])]
        discardCard e
        when (cdAbilityType (eventCardDef e) `elem` map Just [Ability.Interrupt, Ability.HeroInterrupt, Ability.ForcedInterrupt, Ability.Response, Ability.HeroResponse, Ability.ForcedResponse]) $
          checkWindows [W.Window W.After $ W.EventAbilityResolved (eventController e) (eventId e) (eventCardDef e)]
        pure e {eventAttacked = [], eventThwarted = False}
      -- each attack of an event not labeled one, as Dance of Death's, is answered as made
      EventAttacked attacker enemy ranged -> do
        let card = MkPlayerCard (CardId $ unEventId $ eventId e) (eventCardDef e) (Just $ eventController e) (Just $ eventController e)
        labels <- cdAbilitySubTypes <$> effectivePlayerCardDef (eventController e) card
        -- one attack, however many instances of damage it deals the enemy
        if Ability.Attack `elem` labels
          then pure e {eventAttacked = eventAttacked e <> [(attacker, enemy, ranged) | (attacker, enemy, ranged) `notElem` eventAttacked e]}
          else do
            attackResolved attacker enemy ranged
            checkWindows [W.Window W.After $ W.PlayerAttackResolved (toSource e) attacker [enemy]]
            pure e
      _ -> pure e
     where
      -- a ranged attack ignores retaliate
      attackResolved attacker enemy ranged = do
        unless ranged $ enemyRetaliates enemy (IdentityCharacter attacker)
        checkWindows [W.Window W.After $ W.IdentityAttack attacker enemy]
    _ -> pure e
