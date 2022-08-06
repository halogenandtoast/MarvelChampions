module Marvel.Event where

import Marvel.Prelude

import Data.Typeable
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Events
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Text.Show qualified

data Event = forall a. IsEvent a => Event a

instance Show Event where
  show (Event a) = show a

instance ToJSON Event where
  toJSON (Event a) = toJSON a

instance Eq Event where
  (Event (a :: a)) == (Event (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance FromJSON Event where
  parseJSON v = flip (withObject "Event") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withEventCardCode cCode $ \(_ :: EventCard a) -> Event <$> parseJSON @a v

withEventCardCode
  :: CardCode
  -> (forall a. IsEvent a => EventCard a -> r)
  -> r
withEventCardCode cCode f =
  case lookup cCode allEvents of
    Nothing -> error "invalid event"
    Just (SomeEventCard a) -> f a

data SomeEventCard = forall a. IsEvent a => SomeEventCard (EventCard a)

liftEventCard :: (forall a . EventCard a -> b) -> SomeEventCard -> b
liftEventCard f (SomeEventCard a) = f a

someEventCardCode :: SomeEventCard -> CardCode
someEventCardCode = liftEventCard cbCardCode

allEvents :: HashMap CardCode SomeEventCard
allEvents =
  fromList $
    map
      (toFst someEventCardCode)
      [ SomeEventCard backflip
      , SomeEventCard enhancedSpiderSense
      , SomeEventCard swingingWebKick
      , SomeEventCard crisisInterdiction
      , SomeEventCard photonicBlast
      , SomeEventCard gammaSlam
      , SomeEventCard groundStomp
      , SomeEventCard legalPractice
      , SomeEventCard oneTwoPunch
      , SomeEventCard splitPersonality
      , SomeEventCard repulsorBlast
      , SomeEventCard supersonicPunch
      , SomeEventCard ancestralKnowledge
      , SomeEventCard wakandaForeverA
      , SomeEventCard wakandaForeverB
      , SomeEventCard wakandaForeverC
      , SomeEventCard wakandaForeverD
      , SomeEventCard chaseThemDown
      , SomeEventCard relentlessAssault
      , SomeEventCard uppercut
      , SomeEventCard forJustice
      , SomeEventCard greatResponsibility
      , SomeEventCard getReady
      , SomeEventCard leadFromTheFront
      , SomeEventCard makeTheCall
      , SomeEventCard counterPunch
      , SomeEventCard getBehindMe
      , SomeEventCard emergency
      , SomeEventCard firstAid
      , SomeEventCard haymaker
      ]

lookupEvent :: CardCode -> IdentityId -> EventId -> Event
lookupEvent cardCode identityId eventId = case lookup cardCode allEvents of
  Just (SomeEventCard a) -> Event $ cbCardBuilder a (identityId, eventId)
  Nothing -> error $ "Invalid card code for event " <> show cardCode

instance HasModifiersFor Event where
  getModifiersFor source target (Event a) = getModifiersFor source target a

instance Entity Event where
  type EntityId Event = EventId
  type EntityAttrs Event = EventAttrs
  toId = toId . toAttrs
  toAttrs (Event a) = toAttrs a

instance RunMessage Event where
  runMessage msg (Event a) = Event <$> runMessage msg a
