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
  $ concatMap (\(SomeEventCard a) -> SomeEventCard <$> printings a)
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
  , SomeEventCard fearlessDetermination
  , SomeEventCard heroicStrike
  , SomeEventCard shieldBlock
  , SomeEventCard shieldToss
  , SomeEventCard avengersAssemble
  , SomeEventCard strengthInNumbers
  , SomeEventCard expertDefense
  , SomeEventCard bigHands
  , SomeEventCard sneakBy
  , SomeEventCard wiggleRoom
  , SomeEventCard preemptiveStrike
  , SomeEventCard tackle
  , SomeEventCard melee
  , SomeEventCard concussiveBlow
  , SomeEventCard moraleBoost
  , SomeEventCard defenderOfTheNineRealms
  , SomeEventCard forAsgard
  , SomeEventCard hammerThrow
  , SomeEventCard lightningStrike
  , SomeEventCard getOverHere
  , SomeEventCard meanSwing
  , SomeEventCard invulnerability
  , SomeEventCard teamwork
  , SomeEventCard secondWind
  ]

lookupEvent :: CardCode -> IdentityId -> EventId -> Event
lookupEvent cardCode identityId eventId = case lookup cardCode allEvents of
  Just (SomeEventCard a) -> Event $ buildCard a (identityId, eventId)
  Nothing -> error $ "Invalid card code for event " <> show cardCode
