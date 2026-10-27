module Marvel.MainScheme.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Name

allMainSchemes :: HashMap CardCode CardDef
allMainSchemes =
  fromList $ map (toFst toCardCode)
    [ theBreakIn
    , undergroundDistribution
    , secretRendezvous
    , theCrimsonCowl
    , assaultOnNorad
    , countdownToOblivion
    -- The Green Goblin
    , hostileTakeover
    , corporateAcquisition
    , unleashingTheMutagen
    , mutagenCloud

    -- The Wrecking Crew
    , breakout

    -- The Rise of Red Skull
    , attackOnMountAthena
    , theInfinityStone
    , theGetaway
    , noneShallPass
    -- Zola
    , theIslandOfDrZola
    , theMadDoctor

    -- Red Skull
    , theRiseOfRedSkull
    , newWorldHydra

    -- Taskmaster
    , huntingDownHeroes

    -- The Once and Future Kang
    , kangsArrival
    , theMasterOfTime
    , theChronopolis
    , inexorableFate
    , theRealmOfRamaTut
    , thePresentFutureWar
    , kangsWrath

    -- The Galaxy's Most Wanted: Brotherhood of Badoon
    , terrestrialInvasion
    , protectThePlanet

    -- The Galaxy's Most Wanted: Escape the Museum
    , theMissingMilano
    , lostInTheMuseum
    , theGreatEscape

    -- Ronan the Accuser
    , interceptionImminent
    , takeWhatIsMine
    -- The Galaxy's Most Wanted: Nebula
    , theArtOfEvasion
    , warpDriveInitiated
    -- The Galaxy's Most Wanted: Infiltrate the Museum
    , theGrandCollection
    ]

mainScheme :: CardCode -> Name -> EncounterSet -> CardDef
mainScheme code name encounterSet = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just encounterSet
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  }

theBreakIn :: CardDef
theBreakIn = mainScheme "01097" "The Break-In!" Rhino

undergroundDistribution :: CardDef
undergroundDistribution = mainScheme "01116" "Underground Distribution" Klaw

secretRendezvous :: CardDef
secretRendezvous = mainScheme "01117" "Secret Rendezvous" Klaw

theCrimsonCowl :: CardDef
theCrimsonCowl = mainScheme "01137" "The Crimson Cowl" Ultron

assaultOnNorad :: CardDef
assaultOnNorad = mainScheme "01138" "Assault on NORAD" Ultron

countdownToOblivion :: CardDef
countdownToOblivion = mainScheme "01139" "Countdown to Oblivion" Ultron

-- The Green Goblin

hostileTakeover :: CardDef
hostileTakeover = mainScheme "02004" "Hostile Takeover" RiskyBusiness

corporateAcquisition :: CardDef
corporateAcquisition = (mainScheme "02005" "Corporate Acquisition" RiskyBusiness) {cdHazards = 1}

unleashingTheMutagen :: CardDef
unleashingTheMutagen = mainScheme "02017" "Unleashing the Mutagen" MutagenFormula

mutagenCloud :: CardDef
mutagenCloud = mainScheme "02018" "Mutagen Cloud" MutagenFormula
attackOnMountAthena :: CardDef
attackOnMountAthena = mainScheme "04061" "Attack on Mount Athena" Crossbones

theInfinityStone :: CardDef
theInfinityStone = mainScheme "04062" "The Infinity Stone" Crossbones

theGetaway :: CardDef
theGetaway = mainScheme "04063" "The Getaway" Crossbones

noneShallPass :: CardDef
noneShallPass = mainScheme "04079" "None Shall Pass" AbsorbingMan

-- The Rise of Red Skull
-- Zola

theIslandOfDrZola :: CardDef
theIslandOfDrZola = mainScheme "04112" "The Island of Dr. Zola" Zola

theMadDoctor :: CardDef
theMadDoctor = mainScheme "04113" "The Mad Doctor" Zola

-- Red Skull

theRiseOfRedSkull :: CardDef
theRiseOfRedSkull = mainScheme "04128" "The Rise of Red Skull" RedSkull

newWorldHydra :: CardDef
newWorldHydra = mainScheme "04129" "New World Hydra" RedSkull

-- The Rise of Red Skull

-- Taskmaster

huntingDownHeroes :: CardDef
huntingDownHeroes = mainScheme "04096" "Hunting Down Heroes" Taskmaster

-- The Wrecking Crew

breakout :: CardDef
breakout = mainScheme "07001" "Breakout" Breakout

-- The Once and Future Kang

kangsArrival :: CardDef
kangsArrival = mainScheme "11007" "Kang's Arrival" Kang

theMasterOfTime :: CardDef
theMasterOfTime = mainScheme "11008" "The Master of Time" Kang

theChronopolis :: CardDef
theChronopolis = mainScheme "11009" "The Chronopolis" Kang

inexorableFate :: CardDef
inexorableFate = mainScheme "11010" "Inexorable Fate" Kang

theRealmOfRamaTut :: CardDef
theRealmOfRamaTut = mainScheme "11011" "The Realm of Rama-Tut" Kang

thePresentFutureWar :: CardDef
thePresentFutureWar = mainScheme "11012" "The Present Future War" Kang

kangsWrath :: CardDef
kangsWrath = mainScheme "11013" "Kang's Wrath" Kang

-- The Galaxy's Most Wanted: Brotherhood of Badoon

terrestrialInvasion :: CardDef
terrestrialInvasion = mainScheme "16061" "Terrestrial Invasion" BrotherhoodOfBadoon

protectThePlanet :: CardDef
protectThePlanet = mainScheme "16062" "Protect the Planet" BrotherhoodOfBadoon

-- The Galaxy's Most Wanted: Escape the Museum

theMissingMilano :: CardDef
theMissingMilano = mainScheme "16082" "The Missing Milano" EscapeTheMuseum

lostInTheMuseum :: CardDef
lostInTheMuseum = mainScheme "16083" "Lost in the Museum" EscapeTheMuseum

theGreatEscape :: CardDef
theGreatEscape = mainScheme "16084" "The Great Escape" EscapeTheMuseum

-- Ronan the Accuser

interceptionImminent :: CardDef
interceptionImminent = mainScheme "16106" "Interception Imminent" Ronan

takeWhatIsMine :: CardDef
takeWhatIsMine = mainScheme "16107" "\"Take What Is Mine\"" Ronan

-- The Galaxy's Most Wanted: Nebula

theArtOfEvasion :: CardDef
theArtOfEvasion = mainScheme "16091" "The Art of Evasion" Nebula

warpDriveInitiated :: CardDef
warpDriveInitiated = mainScheme "16092" "Warp Drive Initiated" Nebula

-- The Galaxy's Most Wanted: Infiltrate the Museum

theGrandCollection :: CardDef
theGrandCollection = mainScheme "16073" "The Grand Collection" InfiltrateTheMuseum
