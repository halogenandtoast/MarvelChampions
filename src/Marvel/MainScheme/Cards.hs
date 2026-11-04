module Marvel.MainScheme.Cards where

import Marvel.FearNoEvil.Cards qualified as ArtDefs
import Marvel.Shadowland.Cards qualified as ShadowSchemesDefs





import Marvel.CivilWar.MainSchemes qualified as CW
import Marvel.AgentsOfShield.EncounterCards qualified as AosEncounter

import Marvel.TricksterTakeover.Cards qualified as TT

import Marvel.Resource.Types qualified as NEResource ()

import Marvel.Boost qualified as NEBoost ()

import Marvel.GameValue.Types qualified as NEValue ()

import Marvel.EncounterSet qualified as NESet

import Marvel.Keyword qualified as NEKeyword ()

import Marvel.Trait qualified as NETrait ()

import Marvel.Aspect qualified as NEAspect ()

import Marvel.Trait qualified as MojoTrait ()
import Marvel.EncounterSet qualified as MojoSet
import Marvel.Keyword qualified as MojoKeyword ()
import Marvel.GameValue.Types qualified as MojoValue ()
import Marvel.Boost qualified as MojoBoost ()

import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait ()
import Marvel.Keyword qualified as SMKeyword ()
import Marvel.GameValue.Types qualified as SMValue ()
import Marvel.Boost qualified as SMBoost ()

import Marvel.Trait qualified as MGTrait ()
import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword ()
import Marvel.GameValue.Types qualified as MGValue ()
import Marvel.Boost qualified as MGBoost ()
import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Name

allMainSchemes :: HashMap CardCode CardDef
allMainSchemes =
  fromList $ map (toFst toCardCode) $ CW.civilWarMainSchemeDefinitions <>
    [ ArtDefs.street60151b, ArtDefs.street60142b, ArtDefs.street60128b, ArtDefs.street60121b, ShadowSchemesDefs.street65001b, ShadowSchemesDefs.street65002b, ShadowSchemesDefs.street65038b, ShadowSchemesDefs.street65039b, ShadowSchemesDefs.street65060b, ShadowSchemesDefs.street65061b, TT.tt55004a, TT.tt55004b, TT.tt55005a, TT.tt55005b, TT.tt55028a, TT.tt55028b, TT.tt55033a, TT.tt55033b
  , aos50067b, aos50087b, aos50088b, aos50089b, aos50104b, aos50130b, aos50167b, aos50168b, aos50169b
  , huntingGeneTraitors, theHorsemenOfApocalypse, theAgeOfApocalypse, darkBeastsBogusJourney, enSabahNursPyramid, theRiseOfApocalypse
  , ne40077, ne40078, ne40103, ne40104, ne40121, ne40139, ne40140, ne40141, ne40142, ne40143, ne40166, ne40167
  , mojo39002a, mojo39002b, mojo39015a, mojo39015b, mojo39025a, mojo39025b
  , mgStalkedBySabretooth, mgTheInjuredSenator, mgNightOfTheSentinels, mgTheSentinelFactory, mgMasterMoldsAgenda, mgTheBrotherhoodStrikes, mgTheAtrium, mgTheCafeteria, mgTheBasketballCourt, mgTheCourtyard, mgAsteroidM, mgFactoryOnline, mgTheRuleOfMagnus, smHaplessPedestrians, smLeaveUsAlone, smMazeOfMirrors, smEdgeOfReality, smSinisterSynchronization, smSinisterBeatdown, smSkiesOverNewYork, smLowerManhattan, smMidtownManhattan, smUpperManhattan
  , makingConnections
    , promisedProsperity
    , crimeState
    , theBreakIn
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

    -- The Mad Titan's Shadow: Ebony Maw
    , attackOnKnowhere
    , thePowerStoneScheme

    -- The Mad Titan's Shadow: Tower Defense
    , underSiege
    , theArmiesOfThanos

    -- The Mad Titan's Shadow: Thanos
    , theInfinityStones
    , balanceTheScales
    -- The Mad Titan's Shadow: Loki
    , allHailKingLoki
    -- The Mad Titan's Shadow: Hela
    , odinsTorment
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
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
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

-- The Mad Titan's Shadow: Ebony Maw

attackOnKnowhere :: CardDef
attackOnKnowhere = mainScheme "21074" "Attack on Knowhere" EbonyMaw

thePowerStoneScheme :: CardDef
thePowerStoneScheme = mainScheme "21075" "The Power Stone" EbonyMaw

-- The Mad Titan's Shadow: Tower Defense

underSiege :: CardDef
underSiege = mainScheme "21098" "Under Siege" TowerDefense

theArmiesOfThanos :: CardDef
theArmiesOfThanos = mainScheme "21099" "The Armies of Thanos" TowerDefense

-- The Mad Titan's Shadow: Thanos

theInfinityStones :: CardDef
theInfinityStones = mainScheme "21114" "The Infinity Stones" Thanos

balanceTheScales :: CardDef
balanceTheScales = mainScheme "21115" "Balance the Scales" Thanos

-- The Mad Titan's Shadow: Loki

allHailKingLoki :: CardDef
allHailKingLoki = mainScheme "21165" "All Hail King Loki" Loki

-- The Mad Titan's Shadow: Hela

odinsTorment :: CardDef
odinsTorment = mainScheme "21138" "Odin's Torment" Hela


-- The Hood

makingConnections :: CardDef
makingConnections = mainScheme "24004" "Making Connections" TheHood

promisedProsperity :: CardDef
promisedProsperity = mainScheme "24005" "Promised Prosperity" TheHood

crimeState :: CardDef
crimeState = mainScheme "24006" "Crime State" TheHood

-- Sinister Motives

smMainSchemeDef :: CardCode -> Name -> SMSet.EncounterSet -> Natural -> CardDef
smMainSchemeDef code name set quantity = CardDef
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

smHaplessPedestrians :: CardDef
smHaplessPedestrians = (smMainSchemeDef "27064" "Hapless Pedestrians" SMSet.Sandman 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smLeaveUsAlone :: CardDef
smLeaveUsAlone = (smMainSchemeDef "27076" "\"Leave Us Alone!\"" SMSet.Venom 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smMazeOfMirrors :: CardDef
smMazeOfMirrors = (smMainSchemeDef "27087" "Maze of Mirrors" SMSet.Mysterio 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smEdgeOfReality :: CardDef
smEdgeOfReality = (smMainSchemeDef "27088" "Edge of Reality" SMSet.Mysterio 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSinisterSynchronization :: CardDef
smSinisterSynchronization = (smMainSchemeDef "27100" "Sinister Synchronization" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSinisterBeatdown :: CardDef
smSinisterBeatdown = (smMainSchemeDef "27101" "Sinister Beatdown" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSkiesOverNewYork :: CardDef
smSkiesOverNewYork = (smMainSchemeDef "27116" "Skies Over New York" SMSet.VenomGoblin 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smLowerManhattan :: CardDef
smLowerManhattan = (smMainSchemeDef "27117" "Lower Manhattan" SMSet.VenomGoblin 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smMidtownManhattan :: CardDef
smMidtownManhattan = (smMainSchemeDef "27118" "Midtown Manhattan" SMSet.VenomGoblin 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smUpperManhattan :: CardDef
smUpperManhattan = (smMainSchemeDef "27119" "Upper Manhattan" SMSet.VenomGoblin 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

-- Mutant Genesis scenario definitions
mgStalkedBySabretooth :: CardDef
mgStalkedBySabretooth = (smMainSchemeDef "32063" ("Stalked by Sabretooth") MGSet.Sabretooth 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgTheInjuredSenator :: CardDef
mgTheInjuredSenator = (smMainSchemeDef "32064" ("The Injured Senator") MGSet.Sabretooth 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgNightOfTheSentinels :: CardDef
mgNightOfTheSentinels = (smMainSchemeDef "32087" ("Night of the Sentinels") MGSet.ProjectWideawake 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgTheSentinelFactory :: CardDef
mgTheSentinelFactory = (smMainSchemeDef "32112" ("The Sentinel Factory") MGSet.MasterMold 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgMasterMoldsAgenda :: CardDef
mgMasterMoldsAgenda = (smMainSchemeDef "32113" ("Master Mold's Agenda") MGSet.MasterMold 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgTheBrotherhoodStrikes :: CardDef
mgTheBrotherhoodStrikes = (smMainSchemeDef "32125" ("The Brotherhood Strikes!") MGSet.MansionAttack 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgTheAtrium :: CardDef
mgTheAtrium = (smMainSchemeDef "32126" ("The Atrium") MGSet.MansionAttack 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgTheCafeteria :: CardDef
mgTheCafeteria = (smMainSchemeDef "32127" ("The Cafeteria") MGSet.MansionAttack 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgTheBasketballCourt :: CardDef
mgTheBasketballCourt = (smMainSchemeDef "32128" ("The Basketball Court") MGSet.MansionAttack 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgTheCourtyard :: CardDef
mgTheCourtyard = (smMainSchemeDef "32129" ("The Courtyard") MGSet.MansionAttack 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgAsteroidM :: CardDef
mgAsteroidM = (smMainSchemeDef "32141" ("Asteroid M") MGSet.Magneto 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgFactoryOnline :: CardDef
mgFactoryOnline = (smMainSchemeDef "32142" ("Factory Online") MGSet.Magneto 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgTheRuleOfMagnus :: CardDef
mgTheRuleOfMagnus = (smMainSchemeDef "32143" ("The Rule of Magnus") MGSet.Magneto 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }


-- Mojo Mania

mojoCardDef :: CardCode -> Name -> MojoSet.EncounterSet -> Natural -> CardDef
mojoCardDef code name set quantity = CardDef
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

mojo39002a :: CardDef
mojo39002a = (mojoCardDef "39002a" "Melee in the Mojo-seum" MojoSet.Magog 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39002b :: CardDef
mojo39002b = (mojoCardDef "39002b" "Melee in the Mojo-seum" MojoSet.Magog 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39015a :: CardDef
mojo39015a = (mojoCardDef "39015a" "Across the Mojoverse" MojoSet.Spiral 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39015b :: CardDef
mojo39015b = (mojoCardDef "39015b" "Across the Mojoverse" MojoSet.Spiral 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39025a :: CardDef
mojo39025a = (mojoCardDef "39025a" "MojoMania" MojoSet.Mojo 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39025b :: CardDef
mojo39025b = (mojoCardDef "39025b" "MojoMania" MojoSet.Mojo 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

-- NeXt Evolution campaign and scenarios

ne40077 :: CardDef
ne40077 = CardDef
  { cdCardCode = "40077"
  , cdName = "Knock, Knock"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
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

ne40078 :: CardDef
ne40078 = CardDef
  { cdCardCode = "40078"
  , cdName = "Mutant Massacre"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
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

ne40103 :: CardDef
ne40103 = CardDef
  { cdCardCode = "40103"
  , cdName = "Gotta Get Away"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.OnTheRun
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

ne40104 :: CardDef
ne40104 = CardDef
  { cdCardCode = "40104"
  , cdName = "Escaping with Hope"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.OnTheRun
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

ne40121 :: CardDef
ne40121 = CardDef
  { cdCardCode = "40121"
  , cdName = "The Unstoppable Juggernaut"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeJuggernaut
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

ne40139 :: CardDef
ne40139 = CardDef
  { cdCardCode = "40139"
  , cdName = "Sinister Intent"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
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

ne40140 :: CardDef
ne40140 = CardDef
  { cdCardCode = "40140"
  , cdName = "Taking Off"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
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

ne40141 :: CardDef
ne40141 = CardDef
  { cdCardCode = "40141"
  , cdName = "Bulking Up"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
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

ne40142 :: CardDef
ne40142 = CardDef
  { cdCardCode = "40142"
  , cdName = "Focusing In"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
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

ne40143 :: CardDef
ne40143 = CardDef
  { cdCardCode = "40143"
  , cdName = "Sinister Ends"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
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

ne40166 :: CardDef
ne40166 = CardDef
  { cdCardCode = "40166"
  , cdName = "Uncontrollable Power"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeStryfe
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

ne40167 :: CardDef
ne40167 = CardDef
  { cdCardCode = "40167"
  , cdName = "Left to Your Fate"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeStryfe
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

-- Age of Apocalypse campaign and scenarios

huntingGeneTraitors :: CardDef
huntingGeneTraitors = CardDef
  { cdCardCode = "45062"
  , cdName = "Hunting Gene Traitors"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaUnus
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

theHorsemenOfApocalypse :: CardDef
theHorsemenOfApocalypse = CardDef
  { cdCardCode = "45085"
  , cdName = "The Horsemen of Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaFourHorsemen
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

theAgeOfApocalypse :: CardDef
theAgeOfApocalypse = CardDef
  { cdCardCode = "45103"
  , cdName = "The Age of Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
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

darkBeastsBogusJourney :: CardDef
darkBeastsBogusJourney = CardDef
  { cdCardCode = "45121"
  , cdName = "Dark Beast's Bogus Journey"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaDarkBeast
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

enSabahNursPyramid :: CardDef
enSabahNursPyramid = CardDef
  { cdCardCode = "45147"
  , cdName = "En Sabah Nur's Pyramid"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaEnSabahNur
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

theRiseOfApocalypse :: CardDef
theRiseOfApocalypse = CardDef
  { cdCardCode = "45148"
  , cdName = "The Rise of Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaEnSabahNur
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

aos50067b :: CardDef
aos50067b = AosEncounter.aos50067b

aos50087b :: CardDef
aos50087b = AosEncounter.aos50087b

aos50088b :: CardDef
aos50088b = AosEncounter.aos50088b

aos50089b :: CardDef
aos50089b = AosEncounter.aos50089b

aos50104b :: CardDef
aos50104b = AosEncounter.aos50104b

aos50130b :: CardDef
aos50130b = AosEncounter.aos50130b

aos50167b :: CardDef
aos50167b = AosEncounter.aos50167b

aos50168b :: CardDef
aos50168b = AosEncounter.aos50168b

aos50169b :: CardDef
aos50169b = AosEncounter.aos50169b
