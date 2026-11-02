module Marvel.SideScheme.Cards where

import Marvel.CivilWar.Cards qualified as CW
import Marvel.CampaignPlayers.EncounterCards qualified as CN
import Marvel.CampaignPlayers.Cards qualified as CampaignPlayers
import Marvel.JessicaJones.Cards qualified as JL
import Marvel.AgentsOfShield.EncounterCards qualified as AosEncounter
import Marvel.FalconWinter.Cards qualified as FalconWinter

import Marvel.Ability.Types qualified as AospAbility ()
import Marvel.Aspect qualified as AospAspect ()
import Marvel.Boost qualified as AospBoost
import Marvel.Card.Def qualified as AospCard
import Marvel.Cost.Types qualified as AospCost
import Marvel.Criteria qualified as AospCriteria
import Marvel.EncounterSet qualified as AospSet
import Marvel.Keyword qualified as AospKeyword ()
import Marvel.Matchers qualified as AospMatch ()
import Marvel.Name qualified as AospName ()
import Marvel.Resource.Types qualified as AospResource ()
import Marvel.Trait qualified as AospTrait ()
import Marvel.Window.Types qualified as AospWindow ()

import Marvel.BlackPantherShuri.Cards qualified as BlackPantherShuriPack
import Marvel.Silk.Cards qualified as SilkPack

import Marvel.TricksterTakeover.Cards qualified as TT

import Marvel.Window.Types qualified as NcWindow ()

import Marvel.Trait.Types qualified as NcTrait ()

import Marvel.Resource.Types qualified as NcResource

import Marvel.Name qualified as NcName ()

import Marvel.Matchers qualified as NcMatch ()

import Marvel.Keyword qualified as NcKeyword

import Marvel.EncounterSet qualified as NcSet

import Marvel.Criteria qualified as NcCriteria

import Marvel.Cost.Types qualified as NcCost

import Marvel.Card.Def qualified as NcCard ()

import Marvel.Boost qualified as NcBoost

import Marvel.Aspect qualified as NcAspect

import Marvel.Ability.Types qualified as NcAbility ()

import Marvel.Jubilee.Cards qualified as Jubilee

import Marvel.Iceman.Cards qualified as IcemanPack
import Marvel.Ability.Types qualified as MgAbility ()
import Marvel.Aspect qualified as MgAspect
import Marvel.Boost qualified as MgBoost
import Marvel.Card.Def qualified as MgCard
import Marvel.Cost.Types qualified as MgCost
import Marvel.Criteria qualified as MgCriteria
import Marvel.EncounterSet qualified as MgSet
import Marvel.Keyword qualified as MgKeyword
import Marvel.Matchers qualified as MgMatch
import Marvel.Name qualified as MgName ()
import Marvel.Resource.Types qualified as MgResource
import Marvel.Trait qualified as MgTrait
import Marvel.Window.Types qualified as MgWindow ()




import Marvel.Boost qualified as AoaMB
import Marvel.EncounterSet qualified as AoaMS
import Marvel.Boost qualified as BpBoost
import Marvel.Card.Def qualified as BpCard
import Marvel.Cost.Types qualified as BpCost
import Marvel.Criteria qualified as BpCriteria
import Marvel.EncounterSet qualified as BpSet

import Marvel.Ability.Types qualified as DpAbility ()
import Marvel.Aspect qualified as DpAspect
import Marvel.Boost qualified as DpBoost
import Marvel.Card.Def qualified as DpCard
import Marvel.Cost.Types qualified as DpCost
import Marvel.Criteria qualified as DpCriteria
import Marvel.EncounterSet qualified as DpSet
import Marvel.Keyword qualified as DpKeyword
import Marvel.Matchers qualified as DpMatch ()
import Marvel.Name qualified as DpName
import Marvel.Resource.Types qualified as DpResource
import Marvel.Trait qualified as DpTrait ()
import Marvel.Window.Types qualified as DpWindow ()

import Marvel.Ability.Types qualified as AnAbility ()
import Marvel.Aspect qualified as AnAspect
import Marvel.Boost qualified as AnBoost
import Marvel.Card.Def qualified as AnCard
import Marvel.Cost.Types qualified as AnCost
import Marvel.Criteria qualified as AnCriteria
import Marvel.EncounterSet qualified as AnSet
import Marvel.Keyword qualified as AnKeyword
import Marvel.Matchers qualified as AnMatch ()
import Marvel.Name qualified as AnName ()
import Marvel.Resource.Types qualified as AnResource
import Marvel.Trait qualified as AnTrait ()
import Marvel.Window.Types qualified as AnWindow ()

import Marvel.Ability.Types qualified as PsyAbility ()
import Marvel.Aspect qualified as PsyAspect
import Marvel.Boost qualified as PsyBoost
import Marvel.Card.Def qualified as PsyCard
import Marvel.Cost.Types qualified as PsyCost
import Marvel.Criteria qualified as PsyCriteria
import Marvel.EncounterSet qualified as PsySet
import Marvel.Keyword qualified as PsyKeyword
import Marvel.Matchers qualified as PsyMatch ()
import Marvel.Name qualified as PsyName ()
import Marvel.Resource.Types qualified as PsyResource
import Marvel.Trait qualified as PsyTrait ()
import Marvel.Window.Types qualified as PsyWindow ()

import Marvel.Hood.Cards (hoodCardDef)
import Marvel.EncounterSet qualified as HoodSet
import Marvel.Trait qualified as HoodTrait ()
import Marvel.Keyword qualified as HoodKeyword
import Marvel.Boost qualified as HoodBoost
import Marvel.GameValue.Types qualified as HoodValue



import Marvel.Resource.Types qualified as NEResource ()

import Marvel.Boost qualified as NEBoost

import Marvel.GameValue.Types qualified as NEValue

import Marvel.EncounterSet qualified as NESet

import Marvel.Keyword qualified as NEKeyword

import Marvel.Trait qualified as NETrait

import Marvel.Aspect qualified as NEAspect ()

import Marvel.Boost qualified as NEB
import Marvel.EncounterSet qualified as NES
import Marvel.Aspect qualified as NEA
import Marvel.Resource.Types qualified as NER
import Marvel.Keyword qualified as NEK
import Marvel.Trait qualified as NET ()
import Marvel.Resource qualified as NE
import Marvel.Keyword qualified as NE
import Marvel.Boost qualified as NE
import Marvel.Aspect qualified as NE
import Marvel.Card.Def qualified as NE
import Marvel.Ability.Types qualified as NEAbility ()

import Marvel.Trait qualified as MojoTrait ()
import Marvel.EncounterSet qualified as MojoSet
import Marvel.Keyword qualified as MojoKeyword
import Marvel.GameValue.Types qualified as MojoValue
import Marvel.Boost qualified as MojoBoost

import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.Trait qualified as MGTrait
import Marvel.Resource qualified as MGResource ()
import Marvel.Boost qualified as MGBoost
import Marvel.EncounterSet qualified as SpdrSet
import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait
import Marvel.Keyword qualified as SMKeyword
import Marvel.GameValue.Types qualified as SMValue
import Marvel.Boost qualified as SMBoost

import Marvel.Cost.Types
import Marvel.Window ()
import Marvel.Matchers ()
import Marvel.GameValue.Types qualified as MGValue
import Marvel.WonderMan.Cards qualified as WM

import Marvel.Hercules.Cards qualified as HC

import Marvel.Prelude


import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Data.HashSet (singleton)
import Marvel.Keyword
import Marvel.EncounterSet
import Marvel.GameValue.Types (GameValue (PerPlayer))
import Marvel.Name

allSideSchemes :: HashMap CardCode CardDef
allSideSchemes = fromList $ concatMap
  toCardCodePairs
  [ CW.cw56082,CW.cw56077,CW.cw56120,CW.cw56072,CW.cw56104,CW.cw56149,CW.cw56179,CW.cw57012,CW.cw57054
  , CN.enc56025, CN.enc56056, CN.enc60034, CN.enc60062
  , judgeJuryExecutioner
  , CampaignPlayers.fne60012, CampaignPlayers.fne60024
  , WM.wm58027, HC.hc59028, HC.hc59038, HC.hc59041, HC.hc59045
  , JL.jl61007, JL.jl61020, JL.jl61028, JL.jl61029, JL.jl61032, JL.jl61039, JL.jl62030
  , TT.tt55006, TT.tt55021, TT.tt55022, TT.tt55045, TT.tt55046, TT.tt55047, TT.tt55048, TT.tt55060, TT.tt55061
  , aos50074, aos50075, aos50081, aos50085, aos50095, aos50101, aos50121, aos50122, aos50127, aos50135, aos50136, aos50140, aos50146, aos50150, aos50154, aos50159, aos50162, aos50173, aos50174, aos50180
  , FalconWinter.fw53031, FalconWinter.fw53041, FalconWinter.fw54029, FalconWinter.fw54036
  , aosP50031, aosP50061
  , BlackPantherShuriPack.goingUndercoverShuri, BlackPantherShuriPack.showofEmpathyShuri, BlackPantherShuriPack.manipulatedMUSICShuri, BlackPantherShuriPack.playingforKeepsShuri, SilkPack.gettheScoopSilk, SilkPack.theGreatHuntSilk, SilkPack.growInvulnerableSilk
  , nc48020, nc48028, nc48033
  , Jubilee.jubilee47003, Jubilee.jubilee47016, Jubilee.jubilee47025, Jubilee.jubilee47031, Jubilee.jubilee47032, Jubilee.jubilee47033
  , keepUpThePressure
  , playingWithFire
  , sauronLives
  , newRecruits, martyrForMutants, theInnerCircle
  , endlessRanks, genePool, targetedForExtermination, theRavagesOfWar, aTimeOfFamine, plagueAndPestilence, theSpecterOfDeath, releaseTheHounds, heartOfTheEmpire, theToweringCitadel, theTyrantsThrone, theApocalypseSolution, theDarkRiders, timeTravelShenanigans, villageUnderAttack, policeState, trialByCombat, sourceOfPower, pluggedIn, giantGrowth, ancientRitual, liberateTheSeattleCoreFront, liberateTheSeattleCoreFinished, evacuateSurvivorsFront, evacuateSurvivorsFinished, sabotageTheSeaWallFront, sabotageTheSeaWallFinished, findLostMutantsFront, findLostMutantsFinished, protectTheProfessorFront, protectTheProfessorFinished, northAmericanSeaWall
  , rulerOfLimbo
  , portalThroughTime
  , liveDangerously, involuntaryProcedures, dreadfulDeeds
  , keepThemBusy, specializedTraining, inTheNameOfVengeance, rallyTheTroops
  , renderMedicalAid, hookLineAndSinker
  , layTheTrap, interdimensionalPlunder
  , hood24014, hood24023, hood24033, hood24042, hood24055, hood24056, hood24057, hood24058, hood24064
      , ne40084, ne40085, ne40086, ne40087, ne40093, ne40101, ne40108, ne40109, ne40117, ne40124, ne40131, ne40134, ne40146, ne40168a, ne40168b, ne40175, ne40176, ne40184, ne40189, ne40190a, ne40191a, ne40192a, ne40193a, ne40194a, ne40195a, ne40202
  , notMyLuckyDay
  , superpowerTraining
  , takeOuttheGuards
  , cable40006, cable40018, cable40019, cable40020, cable40027, cable40033
  , mojo39008, mojo39016, mojo39031, mojo39037, mojo39038, mojo39039, mojo39045, mojo39059
  , mystiquesManipulations, theReavers
  , theAssassinsGuild, heraldOfAvalon
  , leaderOfTheMorlocks, rulerOfTheAstralPlane
  , geneticManipulation
  , theCarbonadiumSynthesizer, seekingVengeance
  , mgFindTheSenator, mgMedicalEmergency, mgFeralRage, mgTheBrotherhood, mgMetamorphicMayhem, mgMutantsAtTheMall, mgAbductionProtocols, mgOperationZeroTolerance, mgRelentlessRobots, mgIntruderAlert, mgInsertVirusProgram, mgProtectTheStudents, mgUnderSiege, mgBoardingParty, mgSabotageMasterMold, mgOrbitalDecay, mgMagneticMayhem, mgMagneticallySealed, mgSeized, mgTheAcolytes, mgNimrodsPortal, mgBastionsMachinations, mgFrightenedPolice, mgEnemyOfMyEnemy, mgFindThePrisoners, mgSurpriseAttack, mgMagnetosFortress
  , smDirtTrap, smTidalSands, smGuardTheBellTower, smLashingOut, smToothAndNail, smLightAtTheEndA, smLightAtTheEndB, smBruteForceBarricade, smFesteringMass, smJoyRide, smPanicInTheStreets, smVolunteerWork, smLimitlessSupply, smCoordinatedEffort, smHiddenInShadow, smTeamworkMakesTheDreamWork, smFoolSParadise, smWeaknessFromWithin, smAnalysisParalysis, smBackAlleyBurglary, smCatInATree, smHenchmenHeist, smOffTheRails, smRubbleRescue
  , unbridledAmbition
  , breakinAndTakin
  , crowdControl
  , bombScare
  , defenseNetwork
  , illegalArmsFactory
  , theImmortalKlaw
  , theMastersOfEvil
  , usurpTheThrone
  , personalChallenge
  , highwayRobbery
  , imminentOverload
  , thePsycheMagnitron
  , droneFactory
  , invasiveAI
  , ultronsImperative
  , underAttack
  , legionsOfHydra
  , theDoomsdayChair
  , hitSquad
  , generationWhy
  , killerForHire
  , familyFeud
  , openTheDarkDimension
  , totalDestruction
  -- The Green Goblin
  , collapsingBridge
  , oscorpManufacturing
  , payoff
  , goblinReinforcements
  , goblinNation
  , overrun
  , aMessOfThings
  , powerDrain
  , runningInterference

  -- The Wrecking Crew
  , dayOfReckoning
  , thunderstruck
  , pileItOn
  , clearTheRoad

  -- The Rise of Red Skull
  , markedForDeath
  , theVipersAmbition
  , crossbonesAssault
  , corneredStaff
  , avalanche
  , superAbsorbingPower
  , hydraPatrol

  -- Zola
  , hydraPrison
  , testSubjects
  , zolasExperiments

  -- Red Skull
  , theRedHouse
  , theSleeperAwakened
  , prisonCamps
  , censorThePast
  , hydraReinforcements
  , massChaos

  -- Taskmaster
  , capturedByHydra
  , taskmastersTrainingCamp

  -- The Once and Future Kang
  , timePortal
  , corruptedTimestream
  , kangsDominion
  , pinnedDown
  , rampage
  , theAnachronauts
  , lightOfCenturiesSphere


  -- Ant-Man
  , techTheft

  -- Wasp
  , mothersOrders

  -- Quicksilver
  , extortionOfSeismicProportion

  -- Scarlet Witch
  , theNextEvolution

  -- Star-Lord
  , buddingCrimeSyndicate

  -- Gamora
  , siblingRivalry

  -- Groot
  , blazingInferno

  -- Rocket Raccoon
  , vendetta

  -- The Galaxy's Most Wanted: Ship Command
  , cannonade
  -- Badoon Headhunter
  , fugitiveRecovery
  -- Brotherhood of Badoon
  , blockade
  , bombardment
  , oppressiveArmada
  , spatialPositioning
  -- Ronan the Accuser
  , cutThePower
  , pincerManeuver
  , superiorTactics

  -- The Galaxy's Most Wanted: Nebula
  , lethalIntent
  -- Space Pirates
  , soundTheAlarms

  -- The Mad Titan's Shadow: Ebony Maw
  , reactorOverload
  -- Black Order
  , theBlackOrder
  -- Armies of Titan
  , landingCraft

  -- The Galaxy's Most Wanted: Challenge
  , badoonBlitz
  , badoonBlitzExpert
  , galleryOfSplendor
  , galleryOfSplendorExpert
  , thereIsNoEscape
  , thereIsNoEscapeExpert
  , guerrillaTactics
  , guerrillaTacticsExpert
  , kreeSupremacy
  , kreeSupremacyExpert

  -- The Galaxy's Most Wanted: Galactic Artifacts
  , hujahdarianMonarchEgg
  , magicalTeapot
  , philosophersStone
  , crystalBall
  -- Drax
  , cullTheWeak

  -- Venom
  , klyntarFrenzy

  -- Spectrum
  , reactorMeltdown

  -- Adam Warlock
  , universalChurchOfTruth

  -- The Mad Titan's Shadow: Tower Defense
  , cityUnderAttack

  -- The Mad Titan's Shadow: Thanos
  , sanctuary
  , theTitansThrone
  -- Children of Thanos
  , tribute
  -- The Mad Titan's Shadow: Loki
  , casketOfAncientWinters
  , warInAsgard
  , madnessOnMidgard
  , openTheBifrost

  -- The Mad Titan's Shadow: Hela
  , gnipahellir
  , hallOfNastrond
  , gjallerbru
  -- Legions of Hel
  , legionsOfHel
  -- Frost Giants
  , unnaturalStorm

  -- The Mad Titan's Shadow: the campaign set
  , secureTheLandingPad
  , securityBreach
  , saveTheShawarmaPlace
  , hackSanctuarysComputer
  , defensiveProtocols
  , findTheNornStones
  , retrieveOdinsArmor
  , openTheDungeons
  , selfPreservation
  , deadlyLightShow
  , powerfulEnchantments
  , ultronUnleashed
  , regenerativeResearch
  , trackingPrey
  , bringTheWar
  , armoredAssault
  , ruleByForce
  , feedbackLoop
  , nefariousTrap
  , huntingTheSpiderTotems
  , giantMonsterAttack
  , grandLarceny
  , rampagingJuggernaut
  , theHellfireClub
  , consumeTheWorld
  ]

sideScheme
  :: CardCode -> Name -> [BoostIcon] -> EncounterSet -> Natural -> CardDef
sideScheme code name boostIcons encounterSet quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just encounterSet
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdBoostIcons = boostIcons
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

breakinAndTakin :: CardDef
breakinAndTakin =
  (sideScheme "01107" "Breakin' & Takin'" [Boost, Boost] Rhino 1)
    { cdHazards = 1
    }

crowdControl :: CardDef
crowdControl = sideScheme "01108" "Crowd Control" [Boost, Boost] Rhino 1

bombScare :: CardDef
bombScare = (sideScheme "01109" "Bomb Scare" [Boost, Boost] BombScare 1)
  { cdAcceleration = 1
  }

defenseNetwork :: CardDef
defenseNetwork = sideScheme "01125" "Defense Network" [Boost, Boost] Klaw 1

illegalArmsFactory :: CardDef
illegalArmsFactory =
  (sideScheme "01126" "Illegal Arms Factory" [Boost, Boost] Klaw 1)
    { cdHazards = 1
    }

theImmortalKlaw :: CardDef
theImmortalKlaw =
  (sideScheme "01127" "The \"Immortal\" Klaw" [] Klaw 1) { cdAcceleration = 1 }

theMastersOfEvil :: CardDef
theMastersOfEvil =
  (sideScheme "01128" "The Masters of Evil" [Boost, Boost] MastersOfEvil 1)
    { cdAcceleration = 1
    }

usurpTheThrone :: CardDef
usurpTheThrone = (sideScheme
                   "01156"
                   "Usurp the Throne"
                   [Boost, Boost, Boost]
                   BlackPantherNemesis
                   1
                 )
  { cdHazards = 1
  }

personalChallenge :: CardDef
personalChallenge =
  sideScheme "01161" "Personal Challenge" [Boost, Boost, Boost] SheHulkNemesis 1

highwayRobbery :: CardDef
highwayRobbery =
  (sideScheme "01166" "Highway Robbery" [Boost, Boost, Boost] SpiderManNemesis 1
    )
    { cdAcceleration = 1
    }

imminentOverload :: CardDef
imminentOverload =
  (sideScheme "01171" "Imminent Overload" [Boost, Boost, Boost] IronManNemesis 1
    )
    { cdAcceleration = 1
    }

thePsycheMagnitron :: CardDef
thePsycheMagnitron = (sideScheme
                       "01176"
                       "The Psyche-Magnitron"
                       [Boost, Boost, Boost]
                       CaptainMarvelNemesis
                       1
                     )
  { cdHazards = 1
  }

droneFactory :: CardDef
droneFactory =
  (sideScheme "01148" "Drone Factory" [Boost, Boost] Ultron 1) {cdAcceleration = 1}

invasiveAI :: CardDef
invasiveAI =
  (sideScheme "01149" "Invasive AI" [Boost, Boost, Boost] Ultron 1) {cdHazards = 1}

ultronsImperative :: CardDef
ultronsImperative =
  (sideScheme "01150" "Ultron's Imperative" [Boost, Boost, Boost] Ultron 1) {cdHazards = 1}

underAttack :: CardDef
underAttack = sideScheme "01151" "Under Attack" [Boost, Boost, Boost] UnderAttack 1

legionsOfHydra :: CardDef
legionsOfHydra =
  (sideScheme "01180" "Legions of Hydra" [Boost, Boost, Boost] LegionsOfHydra 2) {cdHazards = 1}

theDoomsdayChair :: CardDef
theDoomsdayChair =
  (sideScheme "01183" "The Doomsday Chair" [Boost, Boost, Boost] TheDoomsdayChair 2)
    { cdAcceleration = 1
    }

hitSquad :: CardDef
hitSquad =
  (sideScheme "03027" "Hit Squad" [Boost, Boost, Boost] CaptainAmericaNemesis 1)
    { cdAcceleration = 1
    }

generationWhy :: CardDef
generationWhy =
  (sideScheme "05026" "Generation Why?" [Boost, Boost, Boost] MsMarvelNemesis 1)
    { cdHazards = 1
    }

totalDestruction :: CardDef
totalDestruction =
  (sideScheme "10027" "Total Destruction" [Boost, Boost, Boost] HulkNemesis 1)
    { cdHazards = 1
    }

killerForHire :: CardDef
killerForHire =
  (sideScheme "08027" "Killer for Hire" [Boost, Boost, Boost] BlackWidowNemesis 1)
    { cdAcceleration = 1
    }

familyFeud :: CardDef
familyFeud = (sideScheme "06027" "Family Feud" [Boost, Boost, Boost] ThorNemesis 1)
  { cdAcceleration = 1
  }

openTheDarkDimension :: CardDef
openTheDarkDimension =
  (sideScheme "09029" "Open the Dark Dimension" [Boost, Boost, Boost] DoctorStrangeNemesis 1)
    { cdAcceleration = 1
    }

markedForDeath :: CardDef
markedForDeath =
  (sideScheme "04028" "Marked for Death" [Boost, Boost, Boost] HawkeyeNemesis 1)
    { cdAcceleration = 1
    }

theVipersAmbition :: CardDef
theVipersAmbition =
  (sideScheme "04055" "The Viper's Ambition" [Boost, Boost] SpiderWomanNemesis 1)
    { cdHazards = 1
    }

crossbonesAssault :: CardDef
crossbonesAssault =
  (sideScheme "04070" "Crossbones' Assault" [Boost, Boost] Crossbones 2)
    { cdAcceleration = 1
    }

corneredStaff :: CardDef
corneredStaff = sideScheme "04071" "Cornered Staff" [Boost, Boost] Crossbones 1

avalanche :: CardDef
avalanche =
  (sideScheme "04091" "Avalanche!" [Boost, Boost, Boost] AbsorbingMan 2)
    { cdAcceleration = 1
    }

superAbsorbingPower :: CardDef
superAbsorbingPower = sideScheme "04092" "Super Absorbing Power" [Star] AbsorbingMan 1

hydraPatrol :: CardDef
hydraPatrol = sideScheme "04154" "Hydra Patrol" [Boost, Boost] HydraPatrol 1
-- The Rise of Red Skull
-- Zola

hydraPrison :: CardDef
hydraPrison = sideScheme "04122" "Hydra Prison" [] Zola 1

testSubjects :: CardDef
testSubjects = (sideScheme "04123" "Test Subjects" [Boost, Boost] Zola 2) {cdHazards = 1}

zolasExperiments :: CardDef
zolasExperiments =
  (sideScheme "04124" "Zola's Experiments" [Boost, Boost, Boost] Zola 1) {cdAcceleration = 1}

-- Red Skull

theRedHouse :: CardDef
theRedHouse = sideScheme "04139" "The Red House" [] RedSkull 1

theSleeperAwakened :: CardDef
theSleeperAwakened = sideScheme "04140" "The Sleeper Awakened" [] RedSkull 1

prisonCamps :: CardDef
prisonCamps = (sideScheme "04141" "Prison Camps" [] RedSkull 1) {cdAcceleration = 1}

censorThePast :: CardDef
censorThePast = (sideScheme "04142" "Censor the Past" [] RedSkull 1) {cdAcceleration = 1}

hydraReinforcements :: CardDef
hydraReinforcements = (sideScheme "04143" "Hydra Reinforcements" [] RedSkull 1) {cdHazards = 1}

massChaos :: CardDef
massChaos = sideScheme "04144" "Mass Chaos" [] RedSkull 1

-- The Green Goblin

collapsingBridge :: CardDef
collapsingBridge = sideScheme "02009" "Collapsing Bridge" [Star] RiskyBusiness 1

oscorpManufacturing :: CardDef
oscorpManufacturing = (sideScheme "02010" "Oscorp Manufacturing" [Boost, Boost, Boost] RiskyBusiness 2) {cdAcceleration = 1}

payoff :: CardDef
payoff = (sideScheme "02011" "Payoff" [Boost, Star] RiskyBusiness 2) {cdHazards = 1}

goblinReinforcements :: CardDef
goblinReinforcements = (sideScheme "02026" "Goblin Reinforcements" [Boost, Boost] MutagenFormula 2) {cdHazards = 1}

goblinNation :: CardDef
goblinNation = sideScheme "02027" "Goblin Nation" [Star] MutagenFormula 1

overrun :: CardDef
overrun = (sideScheme "02028" "Overrun" [Boost] MutagenFormula 2) {cdAcceleration = 2}
aMessOfThings :: CardDef
aMessOfThings = (sideScheme "02037" "A Mess of Things" [Boost, Boost] AMessOfThings 1) {cdHazards = 1}

powerDrain :: CardDef
powerDrain = (sideScheme "02041" "Power Drain" [Boost, Boost, Boost] PowerDrain 1) {cdAcceleration = 1}

runningInterference :: CardDef
runningInterference = sideScheme "02046" "Running Interference" [Boost] RunningInterference 1

-- The Once and Future Kang

timePortal :: CardDef
timePortal = (sideScheme "11033" "Time Portal" [Boost, Boost] Temporal 1) {cdHazards = 1}

theAnachronauts :: CardDef
theAnachronauts = (sideScheme "11045" "The Anachronauts" [Boost, Boost, Boost] Anachronauts 2) {cdHazards = 1}

lightOfCenturiesSphere :: CardDef
lightOfCenturiesSphere =
  (sideScheme "11050" "Light of Centuries Sphere" [Boost, Boost, Boost] MasterOfTime 1) {cdHazards = 1}

corruptedTimestream :: CardDef
corruptedTimestream = sideScheme "11022" "Corrupted Timestream" [Boost, Boost] Kang 1

kangsDominion :: CardDef
kangsDominion = sideScheme "11023" "Kang's Dominion" [Boost, Boost, Boost] Kang 4

pinnedDown :: CardDef
pinnedDown = sideScheme "11024" "Pinned Down" [Boost, Boost] Kang 1

rampage :: CardDef
rampage = (sideScheme "11025" "Rampage" [Boost, Boost] Kang 2) {cdAcceleration = 1}

-- The Rise of Red Skull

-- Taskmaster

capturedByHydra :: CardDef
capturedByHydra = (sideScheme "04107" "Captured by Hydra" [Boost, Boost] Taskmaster 4) {cdAcceleration = 1}

taskmastersTrainingCamp :: CardDef
taskmastersTrainingCamp = (sideScheme "04108" "Taskmaster's Training Camp" [Boost, Boost, Boost] Taskmaster 1) {cdHazards = 1}

-- The Wrecking Crew

dayOfReckoning :: CardDef
dayOfReckoning = sideScheme "07004" "Day of Reckoning" [] Wrecker 1

thunderstruck :: CardDef
thunderstruck = sideScheme "07019" "Thunderstruck" [] Thunderball 1

pileItOn :: CardDef
pileItOn = sideScheme "07034" "Pile It On!" [] Piledriver 1

clearTheRoad :: CardDef
clearTheRoad = sideScheme "07048" "Clear the Road" [] Bulldozer 1

-- Ant-Man

techTheft :: CardDef
techTheft = sideScheme "12026" "Tech Theft" [Boost, Boost, Boost] AntManNemesis 1

-- Wasp

mothersOrders :: CardDef
mothersOrders = sideScheme "13027" "Mother's Orders" [Boost, Boost, Boost] WaspNemesis 1

-- Quicksilver

extortionOfSeismicProportion :: CardDef
extortionOfSeismicProportion =
  (sideScheme "14025" "Extortion of Seismic Proportion" [Boost, Boost, Boost] QuicksilverNemesis 1)
    { cdKeywords = singleton (Incite 1)
    }

-- Scarlet Witch

theNextEvolution :: CardDef
theNextEvolution = sideScheme "15024" "The Next Evolution" [Boost, Boost] ScarletWitchNemesis 1

-- Star-Lord

buddingCrimeSyndicate :: CardDef
buddingCrimeSyndicate =
  (sideScheme "17025" "Budding Crime Syndicate" [Boost, Boost, Boost] StarLordNemesis 1)
    { cdHazards = 1
    , cdKeywords = singleton (Hinder (PerPlayer 2))
    }

-- Gamora

siblingRivalry :: CardDef
siblingRivalry = sideScheme "18025" "Sibling Rivalry" [Boost, Boost, Boost] GamoraNemesis 1

-- Groot

blazingInferno :: CardDef
blazingInferno = sideScheme "16026" "Blazing Inferno" [Boost, Boost] GrootNemesis 1

-- Rocket Raccoon

vendetta :: CardDef
vendetta = (sideScheme "16054" "Vendetta" [Boost, Boost] RocketRaccoonNemesis 1) {cdAmplify = 1}

-- The Galaxy's Most Wanted: Ship Command

-- | Hinder 3 per player, one amplify icon.
cannonade :: CardDef
cannonade = (sideScheme "16144" "Cannonade" [Boost, Boost] ShipCommand 1) {cdKeywords = singleton (Hinder (PerPlayer 3)), cdAmplify = 1}

-- Badoon Headhunter

fugitiveRecovery :: CardDef
fugitiveRecovery = (sideScheme "16187" "Fugitive Recovery" [Boost, Boost, Boost] BadoonHeadhunter 1) {cdKeywords = fromList [Surge, Hinder (PerPlayer 3)]}

-- The Galaxy's Most Wanted: Brotherhood of Badoon

-- | Hinder 2 per player; a crisis icon (the scheme's).
blockade :: CardDef
blockade = (sideScheme "16066" "Blockade" [Boost] BrotherhoodOfBadoon 1) {cdKeywords = singleton (Hinder (PerPlayer 2))}

bombardment :: CardDef
bombardment = sideScheme "16067" "Bombardment" [Boost, Boost] BrotherhoodOfBadoon 1

-- | Hinder 3 per player, a hazard icon.
oppressiveArmada :: CardDef
oppressiveArmada = (sideScheme "16068" "Oppressive Armada" [Boost] BrotherhoodOfBadoon 1) {cdKeywords = singleton (Hinder (PerPlayer 3)), cdHazards = 1}

-- | One amplify icon.
spatialPositioning :: CardDef
spatialPositioning = (sideScheme "16069" "Spatial Positioning" [Boost, Boost, Boost] BrotherhoodOfBadoon 1) {cdAmplify = 1}

-- Ronan the Accuser

-- | A crisis icon.
cutThePower :: CardDef
cutThePower = sideScheme "16111" "Cut the Power" [Star] Ronan 1

-- | Hinder 2 per player, two acceleration icons.
pincerManeuver :: CardDef
pincerManeuver = (sideScheme "16112" "Pincer Maneuver" [Boost, Boost, Boost] Ronan 2) {cdKeywords = singleton (Hinder (PerPlayer 2)), cdAcceleration = 2}

superiorTactics :: CardDef
superiorTactics = sideScheme "16113" "Superior Tactics" [Boost, Boost] Ronan 1

-- The Galaxy's Most Wanted: Nebula

lethalIntent :: CardDef
lethalIntent = (sideScheme "16099" "Lethal Intent" [] Nebula 2) {cdHazards = 1}

-- Space Pirates

soundTheAlarms :: CardDef
soundTheAlarms = sideScheme "16140" "Sound the Alarms" [Star] SpacePirates 1

-- The Galaxy's Most Wanted: Challenge, revealed by the campaign's Setup instructions: the
-- A side in a standard campaign ("Standard Mode Only"), the B side in an expert one.

-- | Hinder X[per_hero] and Victory 1.
challengeScheme :: CardCode -> Name -> Int -> CardDef
challengeScheme code name hinder = (sideScheme code name [] Challenge 1) {cdKeywords = fromList [Hinder (PerPlayer hinder), Victory 1]}

badoonBlitz :: CardDef
badoonBlitz = challengeScheme "16178a" "Badoon Blitz" 3

badoonBlitzExpert :: CardDef
badoonBlitzExpert = challengeScheme "16178b" "Badoon Blitz" 4

galleryOfSplendor :: CardDef
galleryOfSplendor = challengeScheme "16179a" "Gallery of Splendor" 3

galleryOfSplendorExpert :: CardDef
galleryOfSplendorExpert = challengeScheme "16179b" "Gallery of Splendor" 4

thereIsNoEscape :: CardDef
thereIsNoEscape = challengeScheme "16180a" "\"There is No Escape\"" 3

thereIsNoEscapeExpert :: CardDef
thereIsNoEscapeExpert = challengeScheme "16180b" "\"There is No Escape\"" 4

guerrillaTactics :: CardDef
guerrillaTactics = challengeScheme "16181a" "Guerrilla Tactics" 3

guerrillaTacticsExpert :: CardDef
guerrillaTacticsExpert = challengeScheme "16181b" "Guerrilla Tactics" 4

kreeSupremacy :: CardDef
kreeSupremacy = challengeScheme "16182a" "Kree Supremacy" 3

kreeSupremacyExpert :: CardDef
kreeSupremacyExpert = challengeScheme "16182b" "Kree Supremacy" 4

-- The Galaxy's Most Wanted: Galactic Artifacts, each Hinder 2 per player and Victory 0;
-- the icons are the printed cards' (MarvelCDB misses the teapot's hazard icon)

galacticArtifact :: CardCode -> Name -> CardDef
galacticArtifact code name = (sideScheme code name [Boost] GalacticArtifacts 1) {cdUnique = True, cdKeywords = fromList [Hinder (PerPlayer 2), Victory 0]}

hujahdarianMonarchEgg :: CardDef
hujahdarianMonarchEgg = (galacticArtifact "16127" "Hujahdarian Monarch Egg") {cdAcceleration = 1}

magicalTeapot :: CardDef
magicalTeapot = (galacticArtifact "16128" "Magical Teapot") {cdHazards = 1}

philosophersStone :: CardDef
philosophersStone = (galacticArtifact "16129" "Philosopher's Stone") {cdAmplify = 1}

crystalBall :: CardDef
crystalBall = (galacticArtifact "16130" "Crystal Ball") {cdAcceleration = 1}
-- Drax

-- boost icons from the printed card (MarvelCDB has none for this pack)
cullTheWeak :: CardDef
cullTheWeak = sideScheme "19026" "Cull the Weak" [Boost, Boost] DraxNemesis 1

-- Venom

klyntarFrenzy :: CardDef
klyntarFrenzy =
  (sideScheme "20024" "Klyntar Frenzy" [Boost, Boost, Boost] VenomNemesis 1)
    { cdHazards = 1
    }

-- Spectrum

-- | A crisis icon (the scheme's).
reactorMeltdown :: CardDef
reactorMeltdown = sideScheme "21028" "Reactor Meltdown" [Boost, Boost, Boost] SpectrumNemesis 1

-- Adam Warlock

-- | An acceleration icon; a [star] boost ("Boost: Reveal this card").
universalChurchOfTruth :: CardDef
universalChurchOfTruth = (sideScheme "21068" "Universal Church of Truth" [Star] AdamWarlockNemesis 1) {cdAcceleration = 1}

-- The Mad Titan's Shadow: Tower Defense

-- | Hinder 1 per player; a crisis icon (the scheme's).
cityUnderAttack :: CardDef
cityUnderAttack = (sideScheme "21110" "City Under Attack" [Boost, Boost] TowerDefense 2) {cdKeywords = singleton (Hinder (PerPlayer 1))}

-- The Mad Titan's Shadow: Ebony Maw

-- | A crisis icon (the scheme's).
reactorOverload :: CardDef
reactorOverload = sideScheme "21084" "Reactor Overload" [Boost, Boost, Boost] EbonyMaw 2

-- Black Order

theBlackOrder :: CardDef
theBlackOrder = (sideScheme "21087" "The Black Order" [Boost, Boost] BlackOrderSet 1) {cdHazards = 1}

-- Armies of Titan

landingCraft :: CardDef
landingCraft = (sideScheme "21091" "Landing Craft" [Boost, Boost] ArmiesOfTitan 2) {cdAcceleration = 1}

-- The Mad Titan's Shadow: Thanos

-- | Hinder 1 per player, Victory 1; revealed by The Infinity Stones (1B).
sanctuary :: CardDef
sanctuary = (sideScheme "21116" "Sanctuary" [Boost, Boost, Boost] Thanos 1) {cdKeywords = fromList [Hinder (PerPlayer 1), Victory 1]}

-- | One amplify icon.
theTitansThrone :: CardDef
theTitansThrone = (sideScheme "21124" "The Titan's Throne" [Boost, Boost] Thanos 1) {cdAmplify = 1}

-- Children of Thanos

-- | A crisis icon (the scheme's).
tribute :: CardDef
tribute = sideScheme "21128" "Tribute" [Boost, Boost] ChildrenOfThanos 2

-- The Mad Titan's Shadow: Loki
-- each Hinder 1 per player

lokiScheme :: CardCode -> Name -> [BoostIcon] -> CardDef
lokiScheme code name boosts = (sideScheme code name boosts Loki 1) {cdKeywords = singleton (Hinder (PerPlayer 1))}

casketOfAncientWinters :: CardDef
casketOfAncientWinters = lokiScheme "21166" "Casket of Ancient Winters" [Boost, Boost, Boost]

warInAsgard :: CardDef
warInAsgard = lokiScheme "21167" "War in Asgard" [Boost]

madnessOnMidgard :: CardDef
madnessOnMidgard = lokiScheme "21168" "Madness on Midgard" [Boost, Boost]

openTheBifrost :: CardDef
openTheBifrost = lokiScheme "21169" "Open the Bifrost" [Boost, Boost, Boost, Boost]

-- The Mad Titan's Shadow: Hela: the road through Hel, each set aside until the one
-- before it is defeated

gnipahellir :: CardDef
gnipahellir = (sideScheme "21140" "Gnipahellir" [Boost, Boost] Hela 1) {cdKeywords = fromList [Hinder (PerPlayer 1), Victory 2], cdAcceleration = 1}

hallOfNastrond :: CardDef
hallOfNastrond = (sideScheme "21141" "Hall of Nastrond" [Boost, Boost, Boost, Boost] Hela 1) {cdKeywords = fromList [Hinder (PerPlayer 1), Victory 4], cdHazards = 1}

gjallerbru :: CardDef
gjallerbru = (sideScheme "21142" "Gjallerbru" [Boost, Boost, Boost] Hela 1) {cdKeywords = fromList [Hinder (PerPlayer 1), Victory 3], cdAmplify = 1}

-- Legions of Hel

legionsOfHel :: CardDef
legionsOfHel = sideScheme "21155" "Legions of Hel" [Boost, Boost] LegionsOfHel 2

-- Frost Giants

unnaturalStorm :: CardDef
unnaturalStorm = sideScheme "21159" "Unnatural Storm" [Boost, Boost] FrostGiants 1

-- The Mad Titan's Shadow: the campaign set, each A side flipping into its B side as it is
-- defeated

campaignScheme :: CardCode -> Name -> [BoostIcon] -> [Keyword] -> CardDef
campaignScheme code name boosts keywords = (sideScheme code name boosts MadTitansShadowCampaign 1) {cdKeywords = fromList keywords}

secureTheLandingPad :: CardDef
secureTheLandingPad = campaignScheme "21180a" "Secure the Landing Pad" [] [Hinder (PerPlayer 1)]

securityBreach :: CardDef
securityBreach = campaignScheme "21181" "Security Breach" [Boost, Boost] [Hinder (PerPlayer 2), Victory 2]

saveTheShawarmaPlace :: CardDef
saveTheShawarmaPlace = campaignScheme "21182a" "Save the Shawarma Place" [] [Hinder (PerPlayer 1)]

hackSanctuarysComputer :: CardDef
hackSanctuarysComputer = campaignScheme "21184a" "Hack Sanctuary's Computer" [] [Hinder (PerPlayer 1)]

defensiveProtocols :: CardDef
defensiveProtocols = campaignScheme "21184b" "Defensive Protocols" [] [Hinder (PerPlayer 2), Victory 2]

findTheNornStones :: CardDef
findTheNornStones = campaignScheme "21186a" "Find the Norn Stones" [] []

retrieveOdinsArmor :: CardDef
retrieveOdinsArmor = campaignScheme "21186b" "Retrieve Odin's Armor" [] [Hinder (PerPlayer 2), Victory 1]

openTheDungeons :: CardDef
openTheDungeons = campaignScheme "21189a" "Open the Dungeons" [] []

-- Nebula

selfPreservation :: CardDef
selfPreservation = sideScheme "22029" "Self-Preservation" [Boost, Boost] NebulaNemesis 1

-- War Machine

deadlyLightShow :: CardDef
deadlyLightShow = (sideScheme "23030" "Deadly Light Show" [Boost, Boost, Boost] WarMachineNemesis 1)
    { cdKeywords = fromList [Hinder (PerPlayer 1)]
    }

-- Valkyrie

powerfulEnchantments :: CardDef
powerfulEnchantments = (sideScheme "25030" "Powerful Enchantments" [Boost, Boost] ValkyrieNemesis 1)
    { cdKeywords = fromList [Hinder (PerPlayer 1)]
    , cdAcceleration = 1
    }

-- Vision

ultronUnleashed :: CardDef
ultronUnleashed = (sideScheme "26030" "Ultron Unleashed" [Boost, Boost, Boost] VisionNemesis 1)
    { cdAcceleration = 1
    }


-- The Hood

unbridledAmbition :: CardDef
unbridledAmbition = (sideScheme "24011" "Unbridled Ambition" [Boost, Boost] TheHood 1) {cdKeywords = singleton (Hinder (PerPlayer 2))}


-- Sinister Motives

regenerativeResearch :: CardDef
regenerativeResearch = sideScheme "27026" "Regenerative Research" [Boost, Boost, Boost] GhostSpiderNemesis 1

trackingPrey :: CardDef
trackingPrey = (sideScheme "27057" "Tracking Prey" [Boost, Boost, Boost] SpiderManMoralesNemesis 1)
    { cdAcceleration = 1
    }

-- Sinister Motives

smSideSchemeDef :: CardCode -> Name -> SMSet.EncounterSet -> Natural -> CardDef
smSideSchemeDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just set
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

smDirtTrap :: CardDef
smDirtTrap = (smSideSchemeDef "27068" "Dirt Trap" SMSet.Sandman 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smTidalSands :: CardDef
smTidalSands = (smSideSchemeDef "27069" "Tidal Sands" SMSet.Sandman 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smGuardTheBellTower :: CardDef
smGuardTheBellTower = (smSideSchemeDef "27079" "Guard the Bell Tower" SMSet.Venom 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smLashingOut :: CardDef
smLashingOut = (smSideSchemeDef "27080" "Lashing Out" SMSet.Venom 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

smToothAndNail :: CardDef
smToothAndNail = (smSideSchemeDef "27081" "Tooth and Nail" SMSet.Venom 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Star]
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smLightAtTheEndA :: CardDef
smLightAtTheEndA = (smSideSchemeDef "27102a" "Light at the End" SMSet.SinisterSix 0)
  { cdTraits = fromList [SMTrait.Trap]
  , cdKeywords = fromList [SMKeyword.Permanent, SMKeyword.Hinder (SMValue.PerPlayer 10)]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smLightAtTheEndB :: CardDef
smLightAtTheEndB = (smSideSchemeDef "27102b" "Light at the End" SMSet.SinisterSix 0)
  { cdTraits = fromList [SMTrait.Chase]
  , cdKeywords = fromList [SMKeyword.Permanent, SMKeyword.Hinder (SMValue.PerPlayer 10)]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smBruteForceBarricade :: CardDef
smBruteForceBarricade = (smSideSchemeDef "27107" "Brute Force Barricade" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Victory 1]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smFesteringMass :: CardDef
smFesteringMass = (smSideSchemeDef "27124" "Festering Mass" SMSet.VenomGoblin 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

smJoyRide :: CardDef
smJoyRide = (smSideSchemeDef "27125" "Joy Ride" SMSet.VenomGoblin 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Hinder (SMValue.PerPlayer 2)]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smPanicInTheStreets :: CardDef
smPanicInTheStreets = (smSideSchemeDef "27127" "Panic in the Streets" SMSet.CityInChaos 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

smVolunteerWork :: CardDef
smVolunteerWork = (smSideSchemeDef "27133" "Volunteer Work" SMSet.DownToEarth 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

smLimitlessSupply :: CardDef
smLimitlessSupply = (smSideSchemeDef "27140" "Limitless Supply" SMSet.GoblinGear 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smCoordinatedEffort :: CardDef
smCoordinatedEffort = (smSideSchemeDef "27143" "Coordinated Effort" SMSet.GuerrillaTactics 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smHiddenInShadow :: CardDef
smHiddenInShadow = (smSideSchemeDef "27144" "Hidden in Shadow" SMSet.GuerrillaTactics 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smTeamworkMakesTheDreamWork :: CardDef
smTeamworkMakesTheDreamWork = (smSideSchemeDef "27145" "Teamwork Makes the Dream Work" SMSet.GuerrillaTactics 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smFoolSParadise :: CardDef
smFoolSParadise = (smSideSchemeDef "27155" "Fool's Paradise" SMSet.PersonalNightmare 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Victory 1]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 1
  , cdAcceleration = 1
  , cdAmplify = 0
  }

smWeaknessFromWithin :: CardDef
smWeaknessFromWithin = (smSideSchemeDef "27156" "Weakness from Within" SMSet.PersonalNightmare 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smAnalysisParalysis :: CardDef
smAnalysisParalysis = (smSideSchemeDef "27173" "Analysis Paralysis" SMSet.WhispersOfParanoia 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  }

smBackAlleyBurglary :: CardDef
smBackAlleyBurglary = (smSideSchemeDef "27176" "Back Alley Burglary" SMSet.CommunityService 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Surge, SMKeyword.Victory 1]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smCatInATree :: CardDef
smCatInATree = (smSideSchemeDef "27177" "Cat in a Tree" SMSet.CommunityService 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Surge, SMKeyword.Victory 1]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smHenchmenHeist :: CardDef
smHenchmenHeist = (smSideSchemeDef "27178" "Henchmen Heist" SMSet.CommunityService 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Surge, SMKeyword.Victory 1]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smOffTheRails :: CardDef
smOffTheRails = (smSideSchemeDef "27179" "Off the Rails" SMSet.CommunityService 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Surge, SMKeyword.Victory 1]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smRubbleRescue :: CardDef
smRubbleRescue = (smSideSchemeDef "27180" "Rubble Rescue" SMSet.CommunityService 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Surge, SMKeyword.Victory 1]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

-- Nova

bringTheWar :: CardDef
bringTheWar = (sideScheme "28022" "\"Bring the War!\"" [Boost, Boost] SMSet.NovaNemesis 1)
  { cdHazards = 1
  }

armoredAssault :: CardDef
armoredAssault = sideScheme "28028" "Armored Assault" [Boost, Boost] SMSet.Armadillo 1


-- Ironheart

ruleByForce :: CardDef
ruleByForce = (sideScheme "29029" "Rule by Force" [Boost, Boost] SMSet.IronheartNemesis 1)
  { cdUnique = False
  }

feedbackLoop :: CardDef
feedbackLoop = (sideScheme "29036" "Feedback Loop" [Boost, Boost, Boost] SMSet.ZzzaxSet 1)
  { cdAcceleration = 1
  , cdUnique = False
  }


-- Spider-Ham

nefariousTrap :: CardDef
nefariousTrap = (sideScheme "30025" "Nefarious Trap" [Boost, Boost] SMSet.SpiderHamNemesis 1)
  { cdUnique = False
  , cdHazards = 1
  }

huntingTheSpiderTotems :: CardDef
huntingTheSpiderTotems = (sideScheme "30030" "Hunting the Spider-Totems" [Boost, Boost, Boost] SMSet.Inheritors 1)
  { cdUnique = False
  }


-- SP//dr

giantMonsterAttack :: CardDef
giantMonsterAttack = (sideScheme "31026" "Giant Monster Attack" [Boost, Boost, Boost] SpdrSet.SpdrNemesis 1)
  { cdUnique = False
  }

grandLarceny :: CardDef
grandLarceny = (sideScheme "31030" "Grand Larceny" [Boost, Boost, Boost] SpdrSet.IronSpiderSinisterSix 1)
  { cdUnique = False
  }

-- Mutant Genesis

rampagingJuggernaut :: CardDef
rampagingJuggernaut = (sideScheme "32027" "Rampaging Juggernaut" [Boost, Boost, Boost] ColossusNemesis 1)

-- Mutant Genesis

theHellfireClub :: CardDef
theHellfireClub = (sideScheme "32057" "The Hellfire Club" [Boost, Boost, Boost] ShadowcatNemesis 1) {cdAcceleration = 1}

mgFrightenedPolice :: CardDef
mgFrightenedPolice = (sideScheme "32171a" "Frightened Police" [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost] MGSet.MutantGenesisCampaign 1) {cdHazards = 0, cdAcceleration = 0}

mgEnemyOfMyEnemy :: CardDef
mgEnemyOfMyEnemy = (sideScheme "32172a" "Enemy of My Enemy" [] MGSet.MutantGenesisCampaign 1) {cdHazards = 0, cdAcceleration = 1}

mgFindThePrisoners :: CardDef
mgFindThePrisoners = (sideScheme "32173a" "Find the Prisoners" [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost] MGSet.MutantGenesisCampaign 1) {cdHazards = 0, cdAcceleration = 0}

mgSurpriseAttack :: CardDef
mgSurpriseAttack = (sideScheme "32174a" "Surprise Attack" [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost] MGSet.MutantGenesisCampaign 1) {cdHazards = 1, cdAcceleration = 0}

mgMagnetosFortress :: CardDef
mgMagnetosFortress = (sideScheme "32175a" "Magneto's Fortress" [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost] MGSet.MutantGenesisCampaign 1) {cdHazards = 0, cdAcceleration = 0}

-- Mutant Genesis scenario definitions
mgFindTheSenator :: CardDef
mgFindTheSenator = (smSideSchemeDef "32065a" ("Find the Senator") MGSet.Sabretooth 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgMedicalEmergency :: CardDef
mgMedicalEmergency = (smSideSchemeDef "32071" ("Medical Emergency") MGSet.Sabretooth 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdAcceleration = 1
  , cdKeywords = fromList [MGKeyword.Victory 1, MGKeyword.Hinder (MGValue.PerPlayer 2)]
  }

mgFeralRage :: CardDef
mgFeralRage = (smSideSchemeDef "32072" ("Feral Rage") MGSet.Sabretooth 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgTheBrotherhood :: CardDef
mgTheBrotherhood = (smSideSchemeDef "32079" ("The Brotherhood") MGSet.Brotherhood 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdHazards = 1
  , cdKeywords = fromList [MGKeyword.Hinder (MGValue.PerPlayer 2)]
  }

mgMetamorphicMayhem :: CardDef
mgMetamorphicMayhem = (smSideSchemeDef "32081" ("Metamorphic Mayhem") MGSet.Mystique 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdAmplify = 1
  , cdKeywords = fromList []
  }

mgMutantsAtTheMall :: CardDef
mgMutantsAtTheMall = (smSideSchemeDef "32088a" ("Mutants at the Mall") MGSet.ProjectWideawake 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdAcceleration = 1
  , cdKeywords = fromList []
  }

mgAbductionProtocols :: CardDef
mgAbductionProtocols = (smSideSchemeDef "32100" ("Abduction Protocols") MGSet.ProjectWideawake 4)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Victory 2, MGKeyword.Hinder (MGValue.PerPlayer 2)]
  }

mgOperationZeroTolerance :: CardDef
mgOperationZeroTolerance = (smSideSchemeDef "32104" ("Operation Zero Tolerance") MGSet.ZeroTolerance 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgRelentlessRobots :: CardDef
mgRelentlessRobots = (smSideSchemeDef "32108" ("Relentless Robots") MGSet.Sentinels 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdHazards = 1
  , cdKeywords = fromList []
  }

mgIntruderAlert :: CardDef
mgIntruderAlert = (smSideSchemeDef "32119" ("Intruder Alert!") MGSet.MasterMold 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgInsertVirusProgram :: CardDef
mgInsertVirusProgram = (smSideSchemeDef "32120" ("Insert Virus Program") MGSet.MasterMold 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdAcceleration = 1
  , cdKeywords = fromList [MGKeyword.Victory 1, MGKeyword.Hinder (MGValue.PerPlayer 2)]
  }

mgProtectTheStudents :: CardDef
mgProtectTheStudents = (smSideSchemeDef "32136" ("Protect the Students") MGSet.MansionAttack 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Hinder (MGValue.PerPlayer 2)]
  }

mgUnderSiege :: CardDef
mgUnderSiege = (smSideSchemeDef "32137" ("Under Siege") MGSet.MansionAttack 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdAcceleration = 1
  , cdKeywords = fromList []
  }

mgBoardingParty :: CardDef
mgBoardingParty = (smSideSchemeDef "32144a" ("Boarding Party") MGSet.Magneto 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgSabotageMasterMold :: CardDef
mgSabotageMasterMold = (smSideSchemeDef "32144b" ("Sabotage Master Mold") MGSet.Magneto 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgOrbitalDecay :: CardDef
mgOrbitalDecay = (smSideSchemeDef "32145a" ("Orbital Decay") MGSet.Magneto 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgMagneticMayhem :: CardDef
mgMagneticMayhem = (smSideSchemeDef "32156" ("Magnetic Mayhem") MGSet.Magneto 1)
  { cdTraits = fromList [MGTrait.Magnetic]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdAmplify = 1
  , cdKeywords = fromList []
  }

mgMagneticallySealed :: CardDef
mgMagneticallySealed = (smSideSchemeDef "32157" ("Magnetically Sealed") MGSet.Magneto 1)
  { cdTraits = fromList [MGTrait.Magnetic]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList []
  }

mgSeized :: CardDef
mgSeized = (smSideSchemeDef "32158" ("Seized!") MGSet.Magneto 1)
  { cdTraits = fromList [MGTrait.Magnetic]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdAcceleration = 1
  , cdKeywords = fromList []
  }

mgTheAcolytes :: CardDef
mgTheAcolytes = (smSideSchemeDef "32165" ("The Acolytes") MGSet.Acolytes 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdHazards = 1
  , cdKeywords = fromList []
  }

mgNimrodsPortal :: CardDef
mgNimrodsPortal = (smSideSchemeDef "32168" ("Nimrod's Portal") MGSet.FuturePast 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdHazards = 1
  , cdKeywords = fromList [MGKeyword.Victory 1]
  }

mgBastionsMachinations :: CardDef
mgBastionsMachinations = (smSideSchemeDef "32169" ("Bastion's Machinations") MGSet.FuturePast 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdAmplify = 1
  , cdKeywords = fromList [MGKeyword.Victory 1]
  }

-- Wolverine

theCarbonadiumSynthesizer :: CardDef
theCarbonadiumSynthesizer = (sideScheme "35029" "The Carbonadium Synthesizer" [Boost, Boost, Boost] WolverineNemesis 1) {cdAcceleration = 1}

seekingVengeance :: CardDef
seekingVengeance = (sideScheme "35035" "Seeking Vengeance" [Boost, Boost, Boost] LadyDeathstrikeSet 1) {cdAcceleration = 1}


-- Phoenix

consumeTheWorld :: CardDef
consumeTheWorld = (sideScheme "34030" "Consume the World" [Boost, Boost, Boost] PhoenixNemesis 1) {cdKeywords = fromList [Permanent], cdAmplify = 1}


-- Cyclops

geneticManipulation :: CardDef
geneticManipulation = (sideScheme "33029" "Genetic Manipulation" [Boost, Boost, Boost] CyclopsNemesis 1) { cdAmplify = 1 }


-- Storm

leaderOfTheMorlocks :: CardDef
leaderOfTheMorlocks = (sideScheme "36032" "Leader of the Morlocks" [Boost, Boost, Boost] StormNemesis 1) { cdAmplify = 1 }

rulerOfTheAstralPlane :: CardDef
rulerOfTheAstralPlane = (sideScheme "36037" "Ruler of the Astral Plane" [Star] ShadowKingSet 1) { cdHazards = 1 }

-- Gambit

theAssassinsGuild :: CardDef
theAssassinsGuild = (sideScheme "37027" "The Assassins Guild" [Boost, Boost, Boost] GambitNemesisSet 1) { cdAcceleration = 1 }

heraldOfAvalon :: CardDef
heraldOfAvalon = (sideScheme "37033" "Herald of Avalon" [Boost, Boost, Boost] ExodusSet 1) { cdHazards = 1 }


-- Rogue

mystiquesManipulations :: CardDef
mystiquesManipulations = (sideScheme "38026" "Mystique's Manipulations" [Boost, Boost] RogueNemesisSet 1) { cdTraits = fromList [MGTrait.Shapeshifter], cdAcceleration = 1 }

theReavers :: CardDef
theReavers = (sideScheme "38034" "The Reavers" [Boost, Boost, Boost] ReaversSet 1) { cdHazards = 1 }


-- Mojo Mania

mojoCardDef :: CardCode -> Name -> MojoSet.EncounterSet -> Natural -> CardDef
mojoCardDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just set
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

mojo39008 :: CardDef
mojo39008 = (mojoCardDef "39008" "Pump Up the Crowd" MojoSet.Magog 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  , cdAcceleration = 2
  }

mojo39016 :: CardDef
mojo39016 = (mojoCardDef "39016" "The Search for Spiral" MojoSet.Spiral 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Permanent]
  , cdBoostIcons = []
  }

mojo39031 :: CardDef
mojo39031 = (mojoCardDef "39031" "Undercover Mojo" MojoSet.Mojo 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Hinder (MojoValue.PerPlayer 2)]
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39037 :: CardDef
mojo39037 = (mojoCardDef "39037" "Crime Scene Investigation" MojoSet.Crime 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Hinder (MojoValue.PerPlayer 1)]
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39038 :: CardDef
mojo39038 = (mojoCardDef "39038" "Law & Order" MojoSet.Crime 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Hinder (MojoValue.PerPlayer 1)]
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39039 :: CardDef
mojo39039 = (mojoCardDef "39039" "Dragnet" MojoSet.Crime 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Hinder (MojoValue.PerPlayer 1)]
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39045 :: CardDef
mojo39045 = (mojoCardDef "39045" "Fetch Quest" MojoSet.Fantasy 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Surge]
  , cdBoostIcons = [MojoBoost.Star]
  }

mojo39059 :: CardDef
mojo39059 = (mojoCardDef "39059" "ICE-Teroid M" MojoSet.SciFi 1)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

-- NeXt Evolution: Cable

cable40006 :: CardDef
cable40006 = (sideScheme "40006" "Technovirus Purge" [] CableNemesis 1) { cdUnique = True, cdDeckLimit = Just 1, cdEncounterSet = Nothing, cdCardType = NE.PlayerSideSchemeType, cdCost = Just 0, cdResources = [(NE.PrintedResource, NE.Energy)], cdAspect = Nothing, cdKeywords = fromList [NE.Victory 0] }

cable40018 :: CardDef
cable40018 = reprintedAs ["59022"] $ (sideScheme "40018" "Call for Backup" [] CableNemesis 1) { cdUnique = True, cdDeckLimit = Just 1, cdEncounterSet = Nothing, cdCardType = NE.PlayerSideSchemeType, cdCost = Just 1, cdResources = [(NE.PrintedResource, NE.Mental)], cdAspect = Just NE.Leadership, cdKeywords = fromList [NE.Victory 0] }

cable40019 :: CardDef
cable40019 = (sideScheme "40019" "Lock and Load" [] CableNemesis 1) { cdUnique = True, cdDeckLimit = Just 1, cdEncounterSet = Nothing, cdCardType = NE.PlayerSideSchemeType, cdCost = Just 1, cdResources = [(NE.PrintedResource, NE.Physical)], cdAspect = Just NE.Aggression, cdKeywords = fromList [NE.Victory 0] }

cable40020 :: CardDef
cable40020 = (sideScheme "40020" "Establish Perimeter" [] CableNemesis 1) { cdUnique = True, cdDeckLimit = Just 1, cdEncounterSet = Nothing, cdCardType = NE.PlayerSideSchemeType, cdCost = Just 1, cdResources = [(NE.PrintedResource, NE.Energy)], cdAspect = Just NE.Protection, cdKeywords = fromList [NE.Victory 0] }

cable40027 :: CardDef
cable40027 = reprintedAs ["51026"] $ (sideScheme "40027" "Build Support" [] CableNemesis 1) { cdUnique = True, cdDeckLimit = Just 1, cdEncounterSet = Nothing, cdCardType = NE.PlayerSideSchemeType, cdCost = Just 1, cdResources = [(NE.PrintedResource, NE.Mental)], cdAspect = Just NE.Neutral, cdKeywords = fromList [NE.Victory 0] }

cable40033 :: CardDef
cable40033 = (sideScheme "40033" "Back to the Future" [NE.Boost, NE.Boost] CableNemesis 1) { cdUnique = False, cdDeckLimit = Just 1 }

-- NeXt Evolution: Domino
takeOuttheGuards :: CardDef
takeOuttheGuards = CardDef
  { cdCardCode = "40054", cdName = "Take Out the Guards", cdCost = Just 0
  , cdTraits = fromList [], cdKeywords = fromList [NEK.Victory 0]
  , cdCardType = PlayerSideSchemeType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just NEA.Justice, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Mental)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
superpowerTraining :: CardDef
superpowerTraining = reprintedAs ["60056"] $  CardDef
  { cdCardCode = "40059", cdName = "Superpower Training", cdCost = Just 1
  , cdTraits = fromList [], cdKeywords = fromList [NEK.Victory 0]
  , cdCardType = PlayerSideSchemeType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just NEA.Neutral, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Energy)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
notMyLuckyDay :: CardDef
notMyLuckyDay = CardDef
  { cdCardCode = "40067", cdName = "Not My Lucky Day", cdCost = Nothing
  , cdTraits = fromList [], cdKeywords = fromList []
  , cdCardType = SideSchemeType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [], cdBoostIcons = [NEB.Boost, NEB.Boost]
  , cdEncounterSet = Just NES.DominoNemesis, cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 1
  , cdLimit = Nothing, cdDeckLimit = Nothing, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution campaign and scenarios

ne40084 :: CardDef
ne40084 = CardDef
  { cdCardCode = "40084"
  , cdName = "By Any Means"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MorlockSiege
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40085 :: CardDef
ne40085 = CardDef
  { cdCardCode = "40085"
  , cdName = "In the Midst of Chaos"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.MorlockSiege
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40086 :: CardDef
ne40086 = CardDef
  { cdCardCode = "40086"
  , cdName = "Maraudin' Ain't Easy"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MorlockSiege
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40087 :: CardDef
ne40087 = CardDef
  { cdCardCode = "40087"
  , cdName = "Territorial Control"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Assault]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MorlockSiege
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40093 :: CardDef
ne40093 = CardDef
  { cdCardCode = "40093"
  , cdName = "The Senator's Support"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Hinder (NEValue.PerPlayer 1)]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MilitaryGrade
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40101 :: CardDef
ne40101 = CardDef
  { cdCardCode = "40101"
  , cdName = "Mutant Slayers"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantSlayers
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40108 :: CardDef
ne40108 = CardDef
  { cdCardCode = "40108"
  , cdName = "Bushwhack"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.OnTheRun
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40109 :: CardDef
ne40109 = CardDef
  { cdCardCode = "40109"
  , cdName = "Pure Force"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.OnTheRun
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40117 :: CardDef
ne40117 = CardDef
  { cdCardCode = "40117"
  , cdName = "Get Nasty"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.NastyBoys
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40124 :: CardDef
ne40124 = CardDef
  { cdCardCode = "40124"
  , cdName = "Building Momentum"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.NeJuggernaut
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 2
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40131 :: CardDef
ne40131 = CardDef
  { cdCardCode = "40131"
  , cdName = "Captive Hope"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.HopeSummersSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40134 :: CardDef
ne40134 = CardDef
  { cdCardCode = "40134"
  , cdName = "Making Green"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Hinder (NEValue.PerPlayer 2)]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.BlackTomCassidy
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40146 :: CardDef
ne40146 = CardDef
  { cdCardCode = "40146"
  , cdName = "Teleported Away"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Hinder (NEValue.PerPlayer 1)]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MisterSinister
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40168a :: CardDef
ne40168a = CardDef
  { cdCardCode = "40168a"
  , cdName = "Stryfe's Grasp"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Permanent, NEKeyword.Hinder (NEValue.PerPlayer 6)]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeStryfe
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40168b :: CardDef
ne40168b = CardDef
  { cdCardCode = "40168b"
  , cdName = "Living Bomb"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Victory (1)]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeStryfe
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40175 :: CardDef
ne40175 = CardDef
  { cdCardCode = "40175"
  , cdName = "Cerebral Erasure"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.NeStryfe
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40176 :: CardDef
ne40176 = CardDef
  { cdCardCode = "40176"
  , cdName = "Telepathic Camouflage"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.NeStryfe
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40184 :: CardDef
ne40184 = CardDef
  { cdCardCode = "40184"
  , cdName = "Extreme Measures"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Hinder (NEValue.PerPlayer 2)]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.ExtremeMeasures
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40189 :: CardDef
ne40189 = CardDef
  { cdCardCode = "40189"
  , cdName = "Mutant Insurrection"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Assault]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantInsurrection
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40190a :: CardDef
ne40190a = CardDef
  { cdCardCode = "40190a"
  , cdName = "Assemble the Team"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40191a :: CardDef
ne40191a = CardDef
  { cdCardCode = "40191a"
  , cdName = "Establish Safehouse"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40192a :: CardDef
ne40192a = CardDef
  { cdCardCode = "40192a"
  , cdName = "Gear Up"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40193a :: CardDef
ne40193a = CardDef
  { cdCardCode = "40193a"
  , cdName = "Mission Prep"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40194a :: CardDef
ne40194a = CardDef
  { cdCardCode = "40194a"
  , cdName = "Practice Maneuvers"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40195a :: CardDef
ne40195a = CardDef
  { cdCardCode = "40195a"
  , cdName = "Prepare Defenses"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40202 :: CardDef
ne40202 = CardDef
  { cdCardCode = "40202"
  , cdName = "Under Pressure"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Surge]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

hood24014 :: CardDef
hood24014 = (hoodCardDef "24014" "Beast Mode" HoodSet.BeastyBoys SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24023 :: CardDef
hood24023 = (hoodCardDef "24023" "Out for Blood" HoodSet.CrossfiresCrew SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Star]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 1
  }

hood24033 :: CardDef
hood24033 = (hoodCardDef "24033" "Self-Experimentation" HoodSet.MisterHydeSet SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24042 :: CardDef
hood24042 = (hoodCardDef "24042" "Crime Pays" HoodSet.SinisterSyndicate SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 1
  , cdHazards = 0
  }

hood24055 :: CardDef
hood24055 = (hoodCardDef "24055" "Feisty Heist" HoodSet.StateOfEmergency SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 1
  , cdHazards = 0
  }

hood24056 :: CardDef
hood24056 = (hoodCardDef "24056" "Disaster at the Docks" HoodSet.StateOfEmergency SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 1
  , cdHazards = 0
  }

hood24057 :: CardDef
hood24057 = (hoodCardDef "24057" "Offshore Inferno" HoodSet.StateOfEmergency SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 1
  , cdHazards = 0
  }

hood24058 :: CardDef
hood24058 = (hoodCardDef "24058" "Hot Pursuit" HoodSet.StateOfEmergency SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 1
  , cdHazards = 0
  }

hood24064 :: CardDef
hood24064 = (hoodCardDef "24064" "Top Talent" HoodSet.WreckingCrewModular SideSchemeType 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [HoodKeyword.Hinder (HoodValue.PerPlayer 2)]
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

-- Side schemes whose printed text has a When Revealed ability, for Citywide Crisis.
hasWhenRevealed :: HasCardCode a => a -> Bool
hasWhenRevealed card = toCardCode card `elem`
  ["01107", "01109", "01125", "01126", "01128", "01148", "01149", "01150", "01151", "01161", "01166", "01171", "01176", "01180", "01183", "02010", "02026", "02037", "02046", "03027", "05026", "06027", "08027", "09029", "04028", "04055", "04071", "04091", "04107", "04122", "04140", "04144", "11022", "11024", "16099", "16113", "21084", "21124", "21155", "21181", "24023", "24033", "24042", "24055", "24056", "24057", "24058", "26030", "27057", "27069", "27079", "27125", "27156", "27173", "29036", "32027", "32137", "32157", "32158", "32169", "32173a", "35035", "39008", "40067", "40084", "40085", "40086", "40087", "40101", "40117", "40131", "40168b", "40175", "40176", "40189", "41027", "44039", "45055", "45117", "47025", "49041", "50031", "50095", "50101", "50121", "50127", "50135", "50136", "50150", "50180", "51033", "52030", "52036", "53041", "55006", "55021", "55022", "55045", "56167", "56183", "57068", "59038", "60062", "61032"]

-- Psylocke

layTheTrap :: CardDef
layTheTrap = reprintedAs ["61021"] $ CardDef
  { cdCardCode = "41016", cdName = "Lay the Trap", cdCost = Just 1
  , cdTraits = fromList [], cdKeywords = fromList [PsyKeyword.Victory 0]
  , cdCardType = PsyCard.PlayerSideSchemeType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just PsyAspect.Justice, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Energy)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

interdimensionalPlunder :: CardDef
interdimensionalPlunder = CardDef
  { cdCardCode = "41027", cdName = "Interdimensional Plunder", cdCost = Nothing
  , cdTraits = fromList [], cdKeywords = fromList []
  , cdCardType = PsyCard.SideSchemeType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [], cdBoostIcons = [PsyBoost.Boost, PsyBoost.Boost, PsyBoost.Boost]
  , cdEncounterSet = Just PsySet.PsylockeNemesis, cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

-- Angel

renderMedicalAid :: CardDef
renderMedicalAid = CardDef
  { cdCardCode = "42017"
  , cdName = "Render Medical Aid"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList [AnKeyword.Victory 0]
  , cdCardType = AnCard.PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AnAspect.Protection
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

hookLineAndSinker :: CardDef
hookLineAndSinker = CardDef
  { cdCardCode = "42026"
  , cdName = "Hook, Line, and Sinker"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AnCard.SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AnBoost.Boost, AnBoost.Boost, AnBoost.Boost]
  , cdEncounterSet = Just AnSet.AngelNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

-- X-23

keepThemBusy :: CardDef
keepThemBusy = CardDef
  { cdCardCode = "43018"
  , cdName = "Keep Them Busy"
  , cdCost = Just 1
  , cdTraits = fromList []
  , cdKeywords = fromList [AnKeyword.Victory 0, AnKeyword.Assault]
  , cdCardType = AnCard.PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AnAspect.Aggression
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

specializedTraining :: CardDef
specializedTraining = CardDef
  { cdCardCode = "43021"
  , cdName = "Specialized Training"
  , cdCost = Just 1
  , cdTraits = fromList []
  , cdKeywords = fromList [AnKeyword.Victory 0]
  , cdCardType = AnCard.PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AnAspect.Neutral
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

inTheNameOfVengeance :: CardDef
inTheNameOfVengeance = CardDef
  { cdCardCode = "43030"
  , cdName = "In the Name of Vengeance"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AnCard.SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AnBoost.Boost, AnBoost.Boost, AnBoost.Boost]
  , cdEncounterSet = Just AnSet.X23Nemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

rallyTheTroops :: CardDef
rallyTheTroops = CardDef
  { cdCardCode = "43039"
  , cdName = "Rally the Troops"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList [AnKeyword.Victory 0]
  , cdCardType = AnCard.PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AnAspect.Leadership
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

-- Deadpool

liveDangerously :: CardDef
liveDangerously = DpCard.CardDef
  { cdCardCode = "44024"
  , cdName = DpName.Name "Live Dangerously" (Nothing)
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList [DpKeyword.Victory 0]
  , cdCardType = DpCard.PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 1
  , cdAcceleration = 1
  , cdAmplify = 1
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

involuntaryProcedures :: CardDef
involuntaryProcedures = DpCard.CardDef
  { cdCardCode = "44034"
  , cdName = DpName.Name "Involuntary Procedures" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [DpBoost.Boost, DpBoost.Boost, DpBoost.Boost]
  , cdEncounterSet = Just DpSet.DeadpoolNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

dreadfulDeeds :: CardDef
dreadfulDeeds = DpCard.CardDef
  { cdCardCode = "44039"
  , cdName = DpName.Name "Dreadful Deeds" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [DpBoost.Boost, DpBoost.Boost, DpBoost.Boost]
  , cdEncounterSet = Just DpSet.DreadpoolSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Bishop

portalThroughTime :: CardDef
portalThroughTime = CardDef
  { cdCardCode = "45027"
  , cdName = "Portal Through Time"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = BpCard.SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = BpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [BpBoost.Boost, BpBoost.Boost, BpBoost.Boost]
  , cdEncounterSet = Just BpSet.BishopNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = BpCost.NoCost
  }

-- Age of Apocalypse: Magik
rulerOfLimbo :: CardDef
rulerOfLimbo = CardDef
  { cdCardCode = "45055"
  , cdName = "Ruler of Limbo"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [AoaMB.Boost, AoaMB.Boost, AoaMB.Boost]
  , cdEncounterSet = Just AoaMS.MagikNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse campaign and scenarios

endlessRanks :: CardDef
endlessRanks = CardDef
  { cdCardCode = "45068"
  , cdName = "Endless Ranks"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaUnus
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

genePool :: CardDef
genePool = CardDef
  { cdCardCode = "45071"
  , cdName = "Gene Pool"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Permanent, NEKeyword.Setup]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaInfinites
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

targetedForExtermination :: CardDef
targetedForExtermination = CardDef
  { cdCardCode = "45074"
  , cdName = "Targeted for Extermination"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaDystopianNightmare
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

theRavagesOfWar :: CardDef
theRavagesOfWar = CardDef
  { cdCardCode = "45086"
  , cdName = "The Ravages of War"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

aTimeOfFamine :: CardDef
aTimeOfFamine = CardDef
  { cdCardCode = "45087"
  , cdName = "A Time of Famine"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

plagueAndPestilence :: CardDef
plagueAndPestilence = CardDef
  { cdCardCode = "45088"
  , cdName = "Plague and Pestilence"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

theSpecterOfDeath :: CardDef
theSpecterOfDeath = CardDef
  { cdCardCode = "45089"
  , cdName = "The Specter of Death"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

releaseTheHounds :: CardDef
releaseTheHounds = CardDef
  { cdCardCode = "45100"
  , cdName = "Release the Hounds"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaHounds
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

heartOfTheEmpire :: CardDef
heartOfTheEmpire = CardDef
  { cdCardCode = "45104a"
  , cdName = "Heart of the Empire"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaApocalypse
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

theToweringCitadel :: CardDef
theToweringCitadel = CardDef
  { cdCardCode = "45104b"
  , cdName = "The Towering Citadel"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaApocalypse
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 2
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

theTyrantsThrone :: CardDef
theTyrantsThrone = CardDef
  { cdCardCode = "45105a"
  , cdName = "The Tyrant's Throne"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaApocalypse
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 3
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

theApocalypseSolution :: CardDef
theApocalypseSolution = CardDef
  { cdCardCode = "45111"
  , cdName = "The Apocalypse Solution"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaApocalypse
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

theDarkRiders :: CardDef
theDarkRiders = CardDef
  { cdCardCode = "45117"
  , cdName = "The Dark Riders"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Hinder (NEValue.PerPlayer 1)]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaDarkRiders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

timeTravelShenanigans :: CardDef
timeTravelShenanigans = CardDef
  { cdCardCode = "45126"
  , cdName = "Time-Travel Shenanigans"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaDarkBeast
  , cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

villageUnderAttack :: CardDef
villageUnderAttack = CardDef
  { cdCardCode = "45132"
  , cdName = "Village Under Attack"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Hinder (NEValue.PerPlayer 1)]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaSavageLand
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

policeState :: CardDef
policeState = CardDef
  { cdCardCode = "45138"
  , cdName = "Police State"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Hinder (NEValue.PerPlayer 1)]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaGenosha
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

trialByCombat :: CardDef
trialByCombat = CardDef
  { cdCardCode = "45146"
  , cdName = "Trial by Combat"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Hinder (NEValue.PerPlayer 1)]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaBlueMoon
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

sourceOfPower :: CardDef
sourceOfPower = CardDef
  { cdCardCode = "45153"
  , cdName = "Source of Power"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

pluggedIn :: CardDef
pluggedIn = CardDef
  { cdCardCode = "45154"
  , cdName = "Plugged In"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

giantGrowth :: CardDef
giantGrowth = CardDef
  { cdCardCode = "45155"
  , cdName = "Giant Growth"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ancientRitual :: CardDef
ancientRitual = CardDef
  { cdCardCode = "45163"
  , cdName = "Ancient Ritual"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Permanent, NEKeyword.Setup]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaClanAkkaba
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

liberateTheSeattleCoreFront :: CardDef
liberateTheSeattleCoreFront = CardDef
  { cdCardCode = "45166a"
  , cdName = "Liberate the Seattle Core"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mission]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaMission
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

liberateTheSeattleCoreFinished :: CardDef
liberateTheSeattleCoreFinished = CardDef
  { cdCardCode = "45166b"
  , cdName = "Liberate the Seattle Core"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Finished]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaMission
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

evacuateSurvivorsFront :: CardDef
evacuateSurvivorsFront = CardDef
  { cdCardCode = "45167a"
  , cdName = "Evacuate Survivors"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mission]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaMission
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

evacuateSurvivorsFinished :: CardDef
evacuateSurvivorsFinished = CardDef
  { cdCardCode = "45167b"
  , cdName = "Evacuate Survivors"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Finished]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaMission
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

sabotageTheSeaWallFront :: CardDef
sabotageTheSeaWallFront = CardDef
  { cdCardCode = "45168a"
  , cdName = "Sabotage the Sea Wall"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mission]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaMission
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

sabotageTheSeaWallFinished :: CardDef
sabotageTheSeaWallFinished = CardDef
  { cdCardCode = "45168b"
  , cdName = "Sabotage the Sea Wall"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Finished]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaMission
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

findLostMutantsFront :: CardDef
findLostMutantsFront = CardDef
  { cdCardCode = "45169a"
  , cdName = "Find Lost Mutants"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mission]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaMission
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

findLostMutantsFinished :: CardDef
findLostMutantsFinished = CardDef
  { cdCardCode = "45169b"
  , cdName = "Find Lost Mutants"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Finished]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaMission
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

protectTheProfessorFront :: CardDef
protectTheProfessorFront = CardDef
  { cdCardCode = "45170a"
  , cdName = "Protect the Professor"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mission]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaMission
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

protectTheProfessorFinished :: CardDef
protectTheProfessorFinished = CardDef
  { cdCardCode = "45170b"
  , cdName = "Protect the Professor"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Finished]
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaMission
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

northAmericanSeaWall :: CardDef
northAmericanSeaWall = CardDef
  { cdCardCode = "45177"
  , cdName = "North American Sea Wall"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Surge, NEKeyword.Victory (2), NEKeyword.Hinder (NEValue.PerPlayer 2)]
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaCampaign
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Magneto

newRecruits :: CardDef
newRecruits = CardDef
  { cdCardCode = "49020"
  , cdName = "New Recruits"
  , cdCost = Just 0
  , cdTraits = fromList []
  , cdKeywords = fromList [MgKeyword.Victory 0]
  , cdCardType = MgCard.PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just MgAspect.Leadership
  , cdCriteria = MgCriteria.SelfMatches (MgMatch.IdentityWithTrait MgTrait.XMen)
  , cdResources = [(MgCard.PrintedResource, MgResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

martyrForMutants :: CardDef
martyrForMutants = CardDef
  { cdCardCode = "49029"
  , cdName = "Martyr for Mutants"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MgCard.SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [MgBoost.Boost, MgBoost.Boost, MgBoost.Boost]
  , cdEncounterSet = Just MgSet.MagnetoNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

theInnerCircle :: CardDef
theInnerCircle = CardDef
  { cdCardCode = "49041"
  , cdName = "The Inner Circle"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MgCard.SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [MgBoost.Boost, MgBoost.Boost]
  , cdEncounterSet = Just MgSet.HellfireSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

-- Iceman
keepUpThePressure :: CardDef
keepUpThePressure = IcemanPack.keepUpThePressure

-- Iceman
playingWithFire :: CardDef
playingWithFire = IcemanPack.playingWithFire

-- Iceman
sauronLives :: CardDef
sauronLives = IcemanPack.sauronLives

-- Nightcrawler
nc48020 :: CardDef
nc48020 = CardDef
  { cdCardCode = "48020"
  , cdName = "Astonishing X-Men"
  , cdCost = Just 1
  , cdTraits = fromList []
  , cdKeywords = fromList [NcKeyword.Victory 0]
  , cdCardType = PlayerSideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just NcAspect.Protection
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = [(PrintedResource, NcResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48028 :: CardDef
nc48028 = CardDef
  { cdCardCode = "48028"
  , cdName = "Brimstone Dimension"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [NcBoost.Boost, NcBoost.Boost, NcBoost.Boost]
  , cdEncounterSet = Just NcSet.NightcrawlerNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48033 :: CardDef
nc48033 = CardDef
  { cdCardCode = "48033"
  , cdName = "The Crazy Gang"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [NcBoost.Boost, NcBoost.Boost]
  , cdEncounterSet = Just NcSet.CrazyGangSet
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Agents of Shield players

aosP50031 :: CardDef
aosP50031 = CardDef
  { cdCardCode = "50031"
  , cdName = "Army of the Controlled"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AospCard.SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AospBoost.Boost]
  , cdEncounterSet = Just AospSet.MariaHillNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }

aosP50061 :: CardDef
aosP50061 = CardDef
  { cdCardCode = "50061"
  , cdName = "Acquire Infinity Formula"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AospCard.SideSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AospBoost.Boost, AospBoost.Boost, AospBoost.Boost]
  , cdEncounterSet = Just AospSet.NickFuryNemesis
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }

aos50074 :: CardDef
aos50074 = AosEncounter.aos50074

aos50075 :: CardDef
aos50075 = AosEncounter.aos50075

aos50081 :: CardDef
aos50081 = AosEncounter.aos50081

aos50085 :: CardDef
aos50085 = AosEncounter.aos50085

aos50095 :: CardDef
aos50095 = AosEncounter.aos50095

aos50101 :: CardDef
aos50101 = AosEncounter.aos50101

aos50121 :: CardDef
aos50121 = AosEncounter.aos50121

aos50122 :: CardDef
aos50122 = AosEncounter.aos50122

aos50127 :: CardDef
aos50127 = AosEncounter.aos50127

aos50135 :: CardDef
aos50135 = AosEncounter.aos50135

aos50136 :: CardDef
aos50136 = AosEncounter.aos50136

aos50140 :: CardDef
aos50140 = AosEncounter.aos50140

aos50146 :: CardDef
aos50146 = AosEncounter.aos50146

aos50150 :: CardDef
aos50150 = AosEncounter.aos50150

aos50154 :: CardDef
aos50154 = AosEncounter.aos50154

aos50159 :: CardDef
aos50159 = AosEncounter.aos50159

aos50162 :: CardDef
aos50162 = AosEncounter.aos50162

aos50173 :: CardDef
aos50173 = AosEncounter.aos50173

aos50174 :: CardDef
aos50174 = AosEncounter.aos50174

aos50180 :: CardDef
aos50180 = AosEncounter.aos50180

judgeJuryExecutioner :: CardDef
judgeJuryExecutioner = (sideScheme "90002" "Judge, Jury, Executioner" [Star] KreeFanatic 1) {cdAcceleration = 1}
