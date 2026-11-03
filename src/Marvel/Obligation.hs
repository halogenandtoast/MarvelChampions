{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Obligation where

import Marvel.CivilWar.RoyalGuard qualified as SynthRoyal
import Marvel.CivilWar.ScarletTwins qualified as SynthTwins
import Marvel.CivilWar.ChoosingSides qualified as CwChoosing

import Marvel.Obligation.Obligations.JessicaLuke qualified as JL
import Marvel.Obligation.Obligations.AgentsOfShield qualified as AosEncounter
import Marvel.Obligation.Obligations.AgeOfApocalypseStandardIII

import Marvel.Obligation.Obligations.AgeOfApocalypseDystopianNightmare

import Marvel.Obligation.Obligations.AgeOfApocalypseCampaign

import Marvel.Obligation.Obligations.NextEvolutionTelepathySet

import Marvel.Obligation.Obligations.Mojo
import Marvel.Obligation.Obligations.MojoGenres
import Marvel.Obligation.Obligations.MutantGenesisCampaign
import Marvel.CampaignPlayers.Obligations qualified as CN
import Marvel.CivilWar.Unregistered qualified as CW
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
allObligations = fromList $ [("57072",SomeObligationCard SynthRoyal.bloodDebt)] <> [("57063",SomeObligationCard SynthTwins.spellcasting)] <> [(toCardCode card,SomeObligationCard card) | card <- CwChoosing.nowItsPersonal] <>  [(toCardCode card,SomeObligationCard card) | card <- CW.unregisteredSupers] <> map
  (toFst someObligationCardCode)
  [ SomeObligationCard CN.cn56023, SomeObligationCard CN.cn56054, SomeObligationCard CN.cn60032, SomeObligationCard CN.cn60060
  , SomeObligationCard wm58025, SomeObligationCard hc59004, SomeObligationCard hc59035
  , SomeObligationCard JL.jl61030, SomeObligationCard JL.jl61033a, SomeObligationCard JL.jl61033b, SomeObligationCard JL.jl61033c, SomeObligationCard JL.jl62028
  , SomeObligationCard AosEncounter.aos50134, SomeObligationCard AosEncounter.aos50179
  , SomeObligationCard fw53029, SomeObligationCard fw54027
  , SomeObligationCard aosP50029, SomeObligationCard aosP50059
  , SomeObligationCard tChallasShadowShuri, SomeObligationCard silkSenseOverloadSilk
  , SomeObligationCard nc48026
  , SomeObligationCard hotHeaded
  , SomeObligationCard grounded
  , SomeObligationCard oldGrievances
  , SomeObligationCard hunted, SomeObligationCard drawingNear, SomeObligationCard panickedRefugees
  , SomeObligationCard darkchilde
  , SomeObligationCard fearTheFuture
  , SomeObligationCard theMercWithTheMouth
  , SomeObligationCard selfIsolation
  , SomeObligationCard apocalypticInfluence
  , SomeObligationCard bodySwapped
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
