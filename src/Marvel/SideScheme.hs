{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.SideScheme where

import Marvel.SideScheme.SideSchemes.HoodPack qualified as HoodPack

import Marvel.SideScheme.SideSchemes.NextEvolutionNeStryfe

import Marvel.SideScheme.SideSchemes.NextEvolutionOnTheRun

import Marvel.SideScheme.SideSchemes.NextEvolutionNextEvolutionCampaign

import Marvel.SideScheme.SideSchemes.NextEvolutionNastyBoys

import Marvel.SideScheme.SideSchemes.NextEvolutionMutantSlayers

import Marvel.SideScheme.SideSchemes.NextEvolutionMutantInsurrection

import Marvel.SideScheme.SideSchemes.NextEvolutionMorlockSiege

import Marvel.SideScheme.SideSchemes.NextEvolutionMisterSinister

import Marvel.SideScheme.SideSchemes.NextEvolutionMilitaryGrade

import Marvel.SideScheme.SideSchemes.NextEvolutionNeJuggernaut

import Marvel.SideScheme.SideSchemes.NextEvolutionHopeSummersSet

import Marvel.SideScheme.SideSchemes.NextEvolutionExtremeMeasures

import Marvel.SideScheme.SideSchemes.NextEvolutionBlackTomCassidy

import Marvel.SideScheme.SideSchemes.Mojo
import Marvel.SideScheme.SideSchemes.MojoGenres
import Marvel.SideScheme.SideSchemes.MojoOpening
import Marvel.SideScheme.SideSchemes.MutantGenesisCampaign
import Marvel.SideScheme.SideSchemes.TheHood qualified as Hood
import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.SideScheme.SideSchemes
import Marvel.SideScheme.Types

instance FromJSON SideScheme where
  parseJSON = withObject "SideScheme" $ \o -> do
    cardDef <- o .: "sideSchemeCardDef"
    withSideSchemeCardCode (cdCardCode cardDef)
      $ \(_ :: SideSchemeCard a) -> SideScheme <$> parseJSON @a (Object o)

withSideSchemeCardCode
  :: CardCode -> (forall a . IsSideScheme a => SideSchemeCard a -> r) -> r
withSideSchemeCardCode cCode f = case lookup cCode allSideSchemes of
  Nothing -> error "invalid side scheme"
  Just (SomeSideSchemeCard a) -> f a

allSideSchemes :: HashMap CardCode SomeSideSchemeCard
allSideSchemes = fromList $ map
  (toFst someSideSchemeCardCode)
  [ SomeSideSchemeCard keepThemBusy, SomeSideSchemeCard specializedTraining, SomeSideSchemeCard inTheNameOfVengeance, SomeSideSchemeCard rallyTheTroops
  , SomeSideSchemeCard renderMedicalAid, SomeSideSchemeCard hookLineAndSinker
  , SomeSideSchemeCard layTheTrap, SomeSideSchemeCard interdimensionalPlunder
  , SomeSideSchemeCard HoodPack.hood24014, SomeSideSchemeCard HoodPack.hood24023, SomeSideSchemeCard HoodPack.hood24033, SomeSideSchemeCard HoodPack.hood24042, SomeSideSchemeCard HoodPack.hood24055, SomeSideSchemeCard HoodPack.hood24056, SomeSideSchemeCard HoodPack.hood24057, SomeSideSchemeCard HoodPack.hood24058, SomeSideSchemeCard HoodPack.hood24064
  , SomeSideSchemeCard ne40084, SomeSideSchemeCard ne40085, SomeSideSchemeCard ne40086, SomeSideSchemeCard ne40087, SomeSideSchemeCard ne40093, SomeSideSchemeCard ne40101, SomeSideSchemeCard ne40108, SomeSideSchemeCard ne40109, SomeSideSchemeCard ne40117, SomeSideSchemeCard ne40124, SomeSideSchemeCard ne40131, SomeSideSchemeCard ne40134, SomeSideSchemeCard ne40146, SomeSideSchemeCard ne40168a, SomeSideSchemeCard ne40168b, SomeSideSchemeCard ne40175, SomeSideSchemeCard ne40176, SomeSideSchemeCard ne40184, SomeSideSchemeCard ne40189, SomeSideSchemeCard ne40190a, SomeSideSchemeCard ne40191a, SomeSideSchemeCard ne40192a, SomeSideSchemeCard ne40193a, SomeSideSchemeCard ne40194a, SomeSideSchemeCard ne40195a, SomeSideSchemeCard ne40202
  , SomeSideSchemeCard notMyLuckyDay
  , SomeSideSchemeCard superpowerTraining
  , SomeSideSchemeCard takeOuttheGuards
  , SomeSideSchemeCard cable40006, SomeSideSchemeCard cable40018, SomeSideSchemeCard cable40019, SomeSideSchemeCard cable40020, SomeSideSchemeCard cable40027, SomeSideSchemeCard cable40033
  , SomeSideSchemeCard mojo39008, SomeSideSchemeCard mojo39016, SomeSideSchemeCard mojo39031, SomeSideSchemeCard mojo39037, SomeSideSchemeCard mojo39038, SomeSideSchemeCard mojo39039, SomeSideSchemeCard mojo39045, SomeSideSchemeCard mojo39059
  , SomeSideSchemeCard mystiquesManipulations, SomeSideSchemeCard theReavers, SomeSideSchemeCard theAssassinsGuild, SomeSideSchemeCard heraldOfAvalon
  , SomeSideSchemeCard leaderOfTheMorlocks
  , SomeSideSchemeCard rulerOfTheAstralPlane
  , SomeSideSchemeCard geneticManipulation
  , SomeSideSchemeCard theCarbonadiumSynthesizer, SomeSideSchemeCard seekingVengeance
  , SomeSideSchemeCard mgFindTheSenator, SomeSideSchemeCard mgMedicalEmergency, SomeSideSchemeCard mgFeralRage, SomeSideSchemeCard mgTheBrotherhood, SomeSideSchemeCard mgMetamorphicMayhem, SomeSideSchemeCard mgMutantsAtTheMall, SomeSideSchemeCard mgAbductionProtocols, SomeSideSchemeCard mgOperationZeroTolerance, SomeSideSchemeCard mgRelentlessRobots, SomeSideSchemeCard mgIntruderAlert, SomeSideSchemeCard mgInsertVirusProgram, SomeSideSchemeCard mgProtectTheStudents, SomeSideSchemeCard mgUnderSiege, SomeSideSchemeCard mgBoardingParty, SomeSideSchemeCard mgSabotageMasterMold, SomeSideSchemeCard mgOrbitalDecay, SomeSideSchemeCard mgMagneticMayhem, SomeSideSchemeCard mgMagneticallySealed, SomeSideSchemeCard mgSeized, SomeSideSchemeCard mgTheAcolytes, SomeSideSchemeCard mgNimrodsPortal, SomeSideSchemeCard mgBastionsMachinations, SomeSideSchemeCard mgFrightenedPolice, SomeSideSchemeCard mgEnemyOfMyEnemy, SomeSideSchemeCard mgFindThePrisoners, SomeSideSchemeCard mgSurpriseAttack, SomeSideSchemeCard mgMagnetosFortress
  , SomeSideSchemeCard smDirtTrap, SomeSideSchemeCard smTidalSands, SomeSideSchemeCard smGuardTheBellTower, SomeSideSchemeCard smLashingOut, SomeSideSchemeCard smToothAndNail, SomeSideSchemeCard smLightAtTheEndA, SomeSideSchemeCard smLightAtTheEndB, SomeSideSchemeCard smBruteForceBarricade, SomeSideSchemeCard smFesteringMass, SomeSideSchemeCard smJoyRide, SomeSideSchemeCard smPanicInTheStreets, SomeSideSchemeCard smVolunteerWork, SomeSideSchemeCard smLimitlessSupply, SomeSideSchemeCard smCoordinatedEffort, SomeSideSchemeCard smHiddenInShadow, SomeSideSchemeCard smTeamworkMakesTheDreamWork, SomeSideSchemeCard smFoolSParadise, SomeSideSchemeCard smWeaknessFromWithin, SomeSideSchemeCard smAnalysisParalysis, SomeSideSchemeCard smBackAlleyBurglary, SomeSideSchemeCard smCatInATree, SomeSideSchemeCard smHenchmenHeist, SomeSideSchemeCard smOffTheRails, SomeSideSchemeCard smRubbleRescue
  , SomeSideSchemeCard Hood.unbridledAmbition
  , SomeSideSchemeCard breakinAndTakin
  , SomeSideSchemeCard crowdControl
  , SomeSideSchemeCard bombScare
  , SomeSideSchemeCard defenseNetwork
  , SomeSideSchemeCard illegalArmsFactory
  , SomeSideSchemeCard theImmortalKlaw
  , SomeSideSchemeCard theMastersOfEvil
  , SomeSideSchemeCard usurpTheThrone
  , SomeSideSchemeCard personalChallenge
  , SomeSideSchemeCard highwayRobbery
  , SomeSideSchemeCard imminentOverload
  , SomeSideSchemeCard thePsycheMagnitron
  , SomeSideSchemeCard droneFactory
  , SomeSideSchemeCard invasiveAI
  , SomeSideSchemeCard ultronsImperative
  , SomeSideSchemeCard underAttack
  , SomeSideSchemeCard legionsOfHydra
  , SomeSideSchemeCard theDoomsdayChair
  , SomeSideSchemeCard hitSquad
  , SomeSideSchemeCard generationWhy
  , SomeSideSchemeCard killerForHire
  , SomeSideSchemeCard familyFeud
  , SomeSideSchemeCard openTheDarkDimension
  , SomeSideSchemeCard totalDestruction
  -- The Green Goblin
  , SomeSideSchemeCard collapsingBridge
  , SomeSideSchemeCard goblinNation
  , SomeSideSchemeCard goblinReinforcements
  , SomeSideSchemeCard oscorpManufacturing
  , SomeSideSchemeCard overrun
  , SomeSideSchemeCard payoff
  , SomeSideSchemeCard aMessOfThings
  , SomeSideSchemeCard powerDrain
  , SomeSideSchemeCard runningInterference

  -- The Wrecking Crew
  , SomeSideSchemeCard dayOfReckoning
  , SomeSideSchemeCard thunderstruck
  , SomeSideSchemeCard pileItOn
  , SomeSideSchemeCard clearTheRoad

  -- The Rise of Red Skull
  , SomeSideSchemeCard markedForDeath
  , SomeSideSchemeCard theVipersAmbition
  , SomeSideSchemeCard crossbonesAssault
  , SomeSideSchemeCard corneredStaff
  , SomeSideSchemeCard avalanche
  , SomeSideSchemeCard superAbsorbingPower
  , SomeSideSchemeCard hydraPatrol

  -- Zola
  , SomeSideSchemeCard hydraPrison
  , SomeSideSchemeCard testSubjects
  , SomeSideSchemeCard zolasExperiments

  -- Red Skull
  , SomeSideSchemeCard theRedHouse
  , SomeSideSchemeCard theSleeperAwakened
  , SomeSideSchemeCard prisonCamps
  , SomeSideSchemeCard censorThePast
  , SomeSideSchemeCard hydraReinforcements
  , SomeSideSchemeCard massChaos

  -- Taskmaster
  , SomeSideSchemeCard capturedByHydra
  , SomeSideSchemeCard taskmastersTrainingCamp

  -- The Once and Future Kang
  , SomeSideSchemeCard timePortal
  , SomeSideSchemeCard theAnachronauts
  , SomeSideSchemeCard lightOfCenturiesSphere
  , SomeSideSchemeCard corruptedTimestream
  , SomeSideSchemeCard kangsDominion
  , SomeSideSchemeCard pinnedDown
  , SomeSideSchemeCard rampage


  -- Ant-Man
  , SomeSideSchemeCard techTheft

  -- Wasp
  , SomeSideSchemeCard mothersOrders

  -- Quicksilver
  , SomeSideSchemeCard extortionOfSeismicProportion

  -- Scarlet Witch
  , SomeSideSchemeCard theNextEvolution

  -- Star-Lord
  , SomeSideSchemeCard buddingCrimeSyndicate

  -- Gamora
  , SomeSideSchemeCard siblingRivalry

  -- Groot
  , SomeSideSchemeCard blazingInferno

  -- Rocket Raccoon
  , SomeSideSchemeCard vendetta

  -- The Galaxy's Most Wanted
  , SomeSideSchemeCard cannonade
  , SomeSideSchemeCard fugitiveRecovery
  -- Brotherhood of Badoon
  , SomeSideSchemeCard blockade
  , SomeSideSchemeCard bombardment
  , SomeSideSchemeCard oppressiveArmada
  , SomeSideSchemeCard spatialPositioning
  -- Ronan the Accuser
  , SomeSideSchemeCard cutThePower
  , SomeSideSchemeCard pincerManeuver
  , SomeSideSchemeCard superiorTactics
  -- The Galaxy's Most Wanted: Nebula, Space Pirates
  , SomeSideSchemeCard lethalIntent
  , SomeSideSchemeCard soundTheAlarms
  -- The Mad Titan's Shadow: Ebony Maw, Black Order, Armies of Titan
  , SomeSideSchemeCard reactorOverload
  , SomeSideSchemeCard theBlackOrder
  , SomeSideSchemeCard landingCraft

  -- The Galaxy's Most Wanted: Challenge
  , SomeSideSchemeCard badoonBlitz
  , SomeSideSchemeCard badoonBlitzExpert
  , SomeSideSchemeCard galleryOfSplendor
  , SomeSideSchemeCard galleryOfSplendorExpert
  , SomeSideSchemeCard thereIsNoEscape
  , SomeSideSchemeCard thereIsNoEscapeExpert
  , SomeSideSchemeCard guerrillaTactics
  , SomeSideSchemeCard guerrillaTacticsExpert
  , SomeSideSchemeCard kreeSupremacy
  , SomeSideSchemeCard kreeSupremacyExpert

  -- The Galaxy's Most Wanted: Galactic Artifacts
  , SomeSideSchemeCard hujahdarianMonarchEgg
  , SomeSideSchemeCard magicalTeapot
  , SomeSideSchemeCard philosophersStone
  , SomeSideSchemeCard crystalBall
  -- Drax
  , SomeSideSchemeCard cullTheWeak

  -- Venom
  , SomeSideSchemeCard klyntarFrenzy

  -- Spectrum
  , SomeSideSchemeCard reactorMeltdown

  -- Adam Warlock
  , SomeSideSchemeCard universalChurchOfTruth

  -- The Mad Titan's Shadow: Tower Defense
  , SomeSideSchemeCard cityUnderAttack

  -- The Mad Titan's Shadow: Thanos, Children of Thanos
  , SomeSideSchemeCard sanctuary
  , SomeSideSchemeCard theTitansThrone
  , SomeSideSchemeCard tribute

  -- The Mad Titan's Shadow: Loki
  , SomeSideSchemeCard casketOfAncientWinters
  , SomeSideSchemeCard warInAsgard
  , SomeSideSchemeCard madnessOnMidgard
  , SomeSideSchemeCard openTheBifrost

  -- The Mad Titan's Shadow: Hela, Legions of Hel, Frost Giants
  , SomeSideSchemeCard gnipahellir
  , SomeSideSchemeCard hallOfNastrond
  , SomeSideSchemeCard gjallerbru
  , SomeSideSchemeCard legionsOfHelScheme
  , SomeSideSchemeCard unnaturalStorm

  -- The Mad Titan's Shadow: the campaign set
  , SomeSideSchemeCard secureTheLandingPad
  , SomeSideSchemeCard securityBreach
  , SomeSideSchemeCard saveTheShawarmaPlace
  , SomeSideSchemeCard hackSanctuarysComputer
  , SomeSideSchemeCard defensiveProtocols
  , SomeSideSchemeCard findTheNornStones
  , SomeSideSchemeCard retrieveOdinsArmor
  , SomeSideSchemeCard openTheDungeons
  , SomeSideSchemeCard selfPreservation
  , SomeSideSchemeCard deadlyLightShow
  , SomeSideSchemeCard powerfulEnchantments
  , SomeSideSchemeCard ultronUnleashed
  , SomeSideSchemeCard regenerativeResearch
  , SomeSideSchemeCard trackingPrey
  , SomeSideSchemeCard bringTheWar
  , SomeSideSchemeCard armoredAssault
  , SomeSideSchemeCard ruleByForce
  , SomeSideSchemeCard feedbackLoop
  , SomeSideSchemeCard nefariousTrap
  , SomeSideSchemeCard huntingTheSpiderTotems
  , SomeSideSchemeCard giantMonsterAttack
  , SomeSideSchemeCard grandLarceny
  , SomeSideSchemeCard rampagingJuggernaut
  , SomeSideSchemeCard theHellfireClub
  , SomeSideSchemeCard consumeTheWorld
  ]

lookupSideScheme :: CardCode -> SideSchemeId -> SideScheme
lookupSideScheme cardCode = case lookup cardCode allSideSchemes of
  Just (SomeSideSchemeCard a) -> SideScheme <$> buildCard a
  Nothing -> error $ "Invalid card code for side scheme " <> show cardCode
