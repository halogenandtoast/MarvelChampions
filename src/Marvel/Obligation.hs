{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Obligation where

import Marvel.Obligation.Obligations.NextEvolutionTelepathySet

import Marvel.Obligation.Obligations.Mojo
import Marvel.Obligation.Obligations.MojoGenres
import Marvel.Obligation.Obligations.MutantGenesisCampaign
import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.Obligation.Obligations
import Marvel.Obligation.Types

instance FromJSON Obligation where
  parseJSON = withObject "Obligation" $ \o -> do
    cardDef <- o .: "obligationCardDef"
    withObligationCardCode (cdCardCode cardDef)
      $ \(_ :: ObligationCard a) -> Obligation <$> parseJSON @a (Object o)

withObligationCardCode
  :: CardCode -> (forall a . IsObligation a => ObligationCard a -> r) -> r
withObligationCardCode cCode f = case lookup cCode allObligations of
  Nothing -> error "invalid obligation"
  Just (SomeObligationCard a) -> f a

allObligations :: HashMap CardCode SomeObligationCard
allObligations = fromList $ map
  (toFst someObligationCardCode)
  [ SomeObligationCard bodySwapped
  , SomeObligationCard ne40160, SomeObligationCard ne40161
  , SomeObligationCard memoriesofArmageddon
  , SomeObligationCard cable40031
  , SomeObligationCard mojo39030, SomeObligationCard mojo39036, SomeObligationCard mojo39061, SomeObligationCard mojo39062, SomeObligationCard mojo39063, SomeObligationCard mojo39064, SomeObligationCard mojo39065
  , SomeObligationCard deadlyTouch, SomeObligationCard guildBusiness
  , SomeObligationCard claustrophobia
  , SomeObligationCard lostVisor
  , SomeObligationCard pastDemons
  , SomeObligationCard mgReactiveDefense
  , SomeObligationCard mgWarnTheOthers, SomeObligationCard smFriendsAndFamily
  , SomeObligationCard affairsOfState
  , SomeObligationCard legalWork
  , SomeObligationCard evictionNotice
  , SomeObligationCard businessProblems
  , SomeObligationCard familyEmergency
  , SomeObligationCard manOutOfTime
  , SomeObligationCard homeByDawn
  , SomeObligationCard burnNotice
  , SomeObligationCard odinsAnger
  , SomeObligationCard physicalToll
  , SomeObligationCard innerDemons
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , SomeObligationCard criminalPast
  , SomeObligationCard uncertainLoyalties

  -- The Rise of Red Skull: an Expert Campaign Set
  , SomeObligationCard zolasAlgorithm
  , SomeObligationCard medicalEmergency
  , SomeObligationCard martialLaw
  , SomeObligationCard antiHeroPropaganda
  -- The Once and Future Kang
  , SomeObligationCard fearOfKang
  , SomeObligationCard weakened
  , SomeObligationCard stolenMemories
  , SomeObligationCard depowered
  , SomeObligationCard timeTravelHijinks


  -- Ant-Man
  , SomeObligationCard careForCassie

  -- Wasp
  , SomeObligationCard redDreams

  -- Quicksilver
  , SomeObligationCard needForSpeed

  -- Scarlet Witch
  , SomeObligationCard slippingSanity

  -- Star-Lord
  , SomeObligationCard banishment

  -- Gamora
  , SomeObligationCard unfulfilledDestiny

  -- Groot
  , SomeObligationCard wilt

  -- Rocket Raccoon
  , SomeObligationCard crisisOnHalfworld
  -- Drax
  , SomeObligationCard memoriesOfAnotherLife
  -- Venom
  , SomeObligationCard struggleForControl
  -- Spectrum
  , SomeObligationCard lossOfControl
  -- Adam Warlock
  , SomeObligationCard regenerationCycle
  -- The Mad Titan's Shadow: the campaign set
  , SomeObligationCard systemShock
  , SomeObligationCard inferiorityComplex
  , SomeObligationCard equipmentMalfunction
  , SomeObligationCard troubleInOtherworld
  , SomeObligationCard corruptedProgramming
  , SomeObligationCard worriedFather
  , SomeObligationCard keepingSecrets
  , SomeObligationCard weightOfTheWorld
  , SomeObligationCard aMinorSetback
  , SomeObligationCard iReallyWantAHotDog
  , SomeObligationCard inheritedBurden
  , SomeObligationCard homesick
  , SomeObligationCard permanentlyPhased
  , SomeObligationCard burningHunger
  ]

lookupObligation :: CardCode -> ObligationId -> Obligation
lookupObligation cardCode = case lookup cardCode allObligations of
  Just (SomeObligationCard a) -> Obligation <$> buildCard a
  Nothing -> error $ "Invalid card code for obligation " <> show cardCode
