module Marvel.SideScheme.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

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
allSideSchemes = fromList $ map
  (toFst toCardCode)
  [ breakinAndTakin
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
  , cdLimit = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
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
