{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Event where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Event.Events
import Marvel.Event.Types
import Marvel.Id

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o -> do
    cardDef <- o .: "eventCardDef"
    withEventCardCode (cdCardCode cardDef)
      $ \(_ :: EventCard a) -> Event <$> parseJSON @a (Object o)

withEventCardCode
  :: CardCode -> (forall a . IsEvent a => EventCard a -> r) -> r
withEventCardCode cCode f = case lookup cCode allEvents of
  Nothing -> error "invalid event"
  Just (SomeEventCard a) -> f a

allEvents :: HashMap CardCode SomeEventCard
allEvents = fromList $ map
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
