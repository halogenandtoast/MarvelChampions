module Marvel.Support.Cards where

import Marvel.FalconWinter.Cards qualified as FalconWinter

import Marvel.Ability.Types qualified as AospAbility ()
import Marvel.Aspect qualified as AospAspect
import Marvel.Boost qualified as AospBoost ()
import Marvel.Card.Def qualified as AospCard
import Marvel.Cost.Types qualified as AospCost
import Marvel.Criteria qualified as AospCriteria
import Marvel.EncounterSet qualified as AospSet ()
import Marvel.Keyword qualified as AospKeyword ()
import Marvel.Matchers qualified as AospMatch ()
import Marvel.Name qualified as AospName ()
import Marvel.Resource.Types qualified as AospResource
import Marvel.Trait qualified as AospTrait
import Marvel.Window.Types qualified as AospWindow ()

import Marvel.BlackPantherShuri.Cards qualified as BlackPantherShuriPack
import Marvel.Silk.Cards qualified as SilkPack
import Marvel.Window.Types qualified as NcWindow ()

import Marvel.Trait.Types qualified as NcTrait

import Marvel.Resource.Types qualified as NcResource

import Marvel.Name qualified as NcName ()

import Marvel.Matchers qualified as NcMatch ()

import Marvel.Keyword qualified as NcKeyword ()

import Marvel.EncounterSet qualified as NcSet ()

import Marvel.Criteria qualified as NcCriteria

import Marvel.Cost.Types qualified as NcCost

import Marvel.Card.Def qualified as NcCard ()

import Marvel.Boost qualified as NcBoost ()

import Marvel.Aspect qualified as NcAspect ()

import Marvel.Ability.Types qualified as NcAbility ()

import Marvel.Iceman.Cards qualified as IcemanPack
import Marvel.Ability.Types qualified as MgAbility ()
import Marvel.Aspect qualified as MgAspect
import Marvel.Boost qualified as MgBoost ()
import Marvel.Card.Def qualified as MgCard
import Marvel.Cost.Types qualified as MgCost
import Marvel.Criteria qualified as MgCriteria
import Marvel.EncounterSet qualified as MgSet ()
import Marvel.Keyword qualified as MgKeyword ()
import Marvel.Matchers qualified as MgMatch
import Marvel.Name qualified as MgName ()
import Marvel.Resource.Types qualified as MgResource
import Marvel.Trait qualified as MgTrait
import Marvel.Window.Types qualified as MgWindow ()

import Marvel.Card.Def ()

import Marvel.Criteria ()

import Marvel.Cost.Types ()

import Marvel.Matchers qualified as AoaMM
import Marvel.Aspect qualified as AoaMA
import Marvel.Resource.Types qualified as AoaMR
import Marvel.Trait qualified as AoaMT
import Marvel.Ability.Types qualified as DpAbility ()
import Marvel.Aspect qualified as DpAspect
import Marvel.Boost qualified as DpBoost ()
import Marvel.Card.Def qualified as DpCard
import Marvel.Cost.Types qualified as DpCost
import Marvel.Criteria qualified as DpCriteria
import Marvel.EncounterSet qualified as DpSet ()
import Marvel.Keyword qualified as DpKeyword ()
import Marvel.Matchers qualified as DpMatch ()
import Marvel.Name qualified as DpName
import Marvel.Resource.Types qualified as DpResource
import Marvel.Trait qualified as DpTrait
import Marvel.Window.Types qualified as DpWindow ()

import Marvel.Ability.Types qualified as AnAbility ()
import Marvel.Aspect qualified as AnAspect
import Marvel.Boost qualified as AnBoost ()
import Marvel.Card.Def qualified as AnCard
import Marvel.Cost.Types qualified as AnCost
import Marvel.Criteria qualified as AnCriteria
import Marvel.EncounterSet qualified as AnSet ()
import Marvel.Keyword qualified as AnKeyword ()
import Marvel.Matchers qualified as AnMatch ()
import Marvel.Name qualified as AnName ()
import Marvel.Resource.Types qualified as AnResource
import Marvel.Trait qualified as AnTrait
import Marvel.Window.Types qualified as AnWindow ()

import Marvel.Ability.Types qualified as PsyAbility ()
import Marvel.Aspect qualified as PsyAspect
import Marvel.Boost qualified as PsyBoost ()
import Marvel.Card.Def qualified as PsyCard
import Marvel.Cost.Types qualified as PsyCost
import Marvel.Criteria qualified as PsyCriteria
import Marvel.EncounterSet qualified as PsySet ()
import Marvel.Keyword qualified as PsyKeyword ()
import Marvel.Matchers qualified as PsyMatch
import Marvel.Name qualified as PsyName ()
import Marvel.Resource.Types qualified as PsyResource
import Marvel.Trait qualified as PsyTrait
import Marvel.Window.Types qualified as PsyWindow ()



import Marvel.Resource.Types qualified as NEResource

import Marvel.Boost ()

import Marvel.GameValue.Types qualified as NEValue ()

import Marvel.EncounterSet qualified as NESet

import Marvel.Keyword ()

import Marvel.Trait qualified as NETrait

import Marvel.Aspect qualified as NEAspect

import Marvel.Boost qualified as NEB ()
import Marvel.EncounterSet qualified as NES ()
import Marvel.Aspect qualified as NEA ()
import Marvel.Resource.Types qualified as NER
import Marvel.Keyword qualified as NEK ()
import Marvel.Trait qualified as NET
import Marvel.Trait qualified as NE
import Marvel.Resource qualified as NE
import Marvel.Keyword ()
import Marvel.Boost ()
import Marvel.Aspect qualified as NE
import Marvel.Card.Def ()
import Marvel.Ability.Types qualified as NEAbility ()

import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.Trait qualified as MGTrait
import Marvel.Resource qualified as MGResource
import Marvel.Boost qualified as MGBoost ()
import Marvel.Keyword qualified as SpdrKeyword
import Marvel.Keyword qualified as SMKeyword
import Marvel.Keyword
import Marvel.EncounterSet
import Marvel.Ability.Types ()
import Marvel.Cost.Types
import Marvel.Window ()
import Marvel.Prelude


import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Resource
import Marvel.Trait
import Marvel.Matchers

allSupports :: HashMap CardCode CardDef
allSupports = fromList $ concatMap
  toCardCodePairs
  [ FalconWinter.fw53006, FalconWinter.fw53007, FalconWinter.fw53020, FalconWinter.fw53021, FalconWinter.fw53037, FalconWinter.fw54007
  , aosP50008, aosP50009, aosP50015, aosP50017, aosP50018, aosP50019, aosP50020, aosP50028, aosP50040, aosP50041, aosP50055, aosP50056
  , BlackPantherShuriPack.theElephantsTrunkShuri, BlackPantherShuriPack.queenRamondaShuri, BlackPantherShuriPack.theRaftShuri, BlackPantherShuriPack.doraMilajeShuri, BlackPantherShuriPack.targetSpotterShuri, SilkPack.albertMoonSilk, SilkPack.jJonahJamesonSilk, SilkPack.drSinclairSilk
  , nc48003
  , iceWall
  , asteroidM, wontStayDown, childrenOfTheAtom
  , missionTeamFront, missionTeamFinished
  , stepfordCuckoos
  , limbo
  , chimichangaTruck, deadpoolCorpsShip, plotConvenience
  , sisterhood
  , worthingtonIndustries, angelsAerie
  , trainingRegimen, ipac, xBunker
  , ne40197
  , thePaintedLady
  , pipthePug
  , cable40007, cable40008, cable40021, cable40022
  , moiraMacTaggert, medLab
  , theThievesGuild, warRoom
  , clearSkies, hurricane, thunderstorm, blizzard, ororosGarden, uncannyXMen, hangarBay
  , utopia, dangerRoom
  , logansCabin, weaponX, commandCenter
  , mgMetroPD
  , alphaFlightStation
  , auntMay
  , superhumanLawDivision
  , pepperPotts
  , starkTower
  , theGoldenCity
  , tacTeam
  , interrogationRoom
  , surveillanceTeam
  , theTriskellion
  , medTeam
  , avengersMansion
  , helicarrier
  , stevesApartment
  , quinjet
  , avengersTower
  , aamirKhan
  , brunoCarrelli
  , nakiaBahadir
  , safeHouse29
  , quincarrier
  , asgard
  , hallOfHeroes
  , sanctumSanctorum
  , theNightNurse
  , bannersLaboratory
  , beatCop
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , teamTraining
  , jessicaDrewsApartment

  -- The Once and Future Kang


  -- Ant-Man
  , armyOfAnts
  , teamBuildingExercise

  -- Wasp
  , bootCamp

  -- Quicksilver
  , servalIndustries

  -- Scarlet Witch
  , agathaHarkness

  -- Star-Lord
  , badBoy
  , targetPractice
  , citt
  , knowhere

  -- Gamora
  , conditioningRoom

  -- Groot
  , fertileGround
  -- Drax
  , regroup

  -- Venom
  , projectRebirth20
  , crewQuarters

  -- Spectrum
  , mightyAvengers

  -- Adam Warlock
  , soulWorld
  , nebulasShipSupport
  , guardiansOfTheGalaxy
  , defensiveTraining
  , munitionsBunker
  , commandTeam
  , vigilanteTraining
  , valhalla
  , theBifrost
  , leadershipTraining
  , hickoryBranchLane616
  , assaultTraining
  , georgeStacy
  , webOfLifeAndDestiny
  , gankeLee
  , jeffersonDavis
  , fieldAgent
  , governmentLiaison
  , skyDestroyer
  , jesseAlexander
  , championsMobileBunker
  , ronnieWilliams
  , tonyStarkAI
  , rAndDFacility
  , theDailyBeagle
  , spdrSuitInactive
  , auntMayAndUncleBen
  , ejectionProtocol
  , spdrCommand
  , piotrsStudio
  , theXjet
  , kittysRoom
  , xmansion
  , whiteHotRoom
  , cerebro
  ]

support
  :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> Aspect -> CardDef
support code name cost traits resources aspect =
  baseSupport code name cost traits resources (Just aspect)

identitySupport :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
identitySupport code name cost traits resources =
  baseSupport code name cost traits resources Nothing

basicSupport :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
basicSupport code name cost traits resources =
  baseSupport code name cost traits resources (Just Neutral)

baseSupport
  :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> Maybe Aspect -> CardDef
baseSupport code name cost traits resources mAspect = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Just cost
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = map (PrintedResource, ) resources
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

unique :: CardDef -> CardDef
unique def = def { cdUnique = True }

limited :: InPlayLimit -> CardDef -> CardDef
limited limit def = def { cdLimit = Just limit }

auntMay :: CardDef
auntMay = unique $ identitySupport "01006" "Aunt May" 1 [Persona] [Energy]

alphaFlightStation :: CardDef
alphaFlightStation = unique $ identitySupport
  "01015"
  "Alpha Flight Station"
  1
  [Location, Shield]
  [Mental]

superhumanLawDivision :: CardDef
superhumanLawDivision =
  identitySupport "01026" "Superhuman Law Division" 1 [Location] [Physical]

pepperPotts :: CardDef
pepperPotts =
  unique $ identitySupport "01033" "Pepper Potts" 3 [Persona] [Physical]

starkTower :: CardDef
starkTower =
  unique $ identitySupport "01034" "Stark Tower" 2 [Location] [Mental]

theGoldenCity :: CardDef
theGoldenCity = unique
  $ identitySupport "01045" "The Golden City" 2 [Location, Wakanda] [Energy]

tacTeam :: CardDef
tacTeam = reprintedAs ["04042"] $ support "01056" "Tac Team" 3 [Shield] [Energy] Aggression

interrogationRoom :: CardDef
interrogationRoom = reprintedAs ["08015", "04048"] $ limited (MaxPerPlayer 1)
  $ support "01063" "Interrogation Room" 1 [Location] [Energy] Justice

surveillanceTeam :: CardDef
surveillanceTeam = reprintedAs ["27045", "08016"] $
  support "01064" "Surveillance Team" 2 [Shield] [Mental] Justice

theTriskellion :: CardDef
theTriskellion = reprintedAs ["53022"] $ unique
  $ support "01073" "The Triskelion" 1 [Location, Shield] [Energy] Leadership

medTeam :: CardDef
medTeam = reprintedAs ["09018"] $ support "01080" "Med Team" 3 [Shield] [Energy] Protection

avengersMansion :: CardDef
avengersMansion = deckLimited 3 $ reprintedAs ["05022", "06025", "09025", "10023", "21021", "26023"] $ limited (MaxPerPlayer 1)
  $ basicSupport "01091" "Avengers Mansion" 4 [Avenger, Location] [Mental]

helicarrier :: CardDef
helicarrier = reprintedAs ["10024", "29026"] $ limited (MaxPerPlayer 1)
  $ basicSupport "01092" "Helicarrier" 3 [Location, Shield] [Physical]

stevesApartment :: CardDef
stevesApartment =
  unique $ identitySupport "03007" "Steve's Apartment" 1 [Location] [Energy]

quinjet :: CardDef
quinjet = support "03019" "Quinjet" 1 [Avenger, Vehicle] [Energy] Leadership

avengersTower :: CardDef
avengersTower = reprintedAs ["04021", "21020"] $
  unique $ basicSupport "03024" "Avengers Tower" 2 [Avenger, Location] [Mental]

aamirKhan :: CardDef
aamirKhan = unique $ identitySupport "05006" "Aamir Khan" 1 [Persona] [Mental]

brunoCarrelli :: CardDef
brunoCarrelli = unique $ identitySupport "05007" "Bruno Carrelli" 1 [Persona] [Physical]

nakiaBahadir :: CardDef
nakiaBahadir = unique $ identitySupport "05008" "Nakia Bahadir" 1 [Persona] [Energy]
bannersLaboratory :: CardDef
bannersLaboratory =
  unique $ identitySupport "10008" "Banner's Laboratory" 2 [Location] [Mental]

beatCop :: CardDef
beatCop = support "10029" "Beat Cop" 3 [Persona] [Physical] Justice

safeHouse29 :: CardDef
safeHouse29 =
  unique $ identitySupport "08005" "Safe House #29" 1 [Location, Shield] [Physical]

quincarrier :: CardDef
quincarrier = deckLimited 1 $ reprintedAs ["13025", "23023"] $ (unique $ basicSupport "08023" "Quincarrier" 3 [Avenger, Vehicle] [Mental])
  { cdCriteria = SelfMatches (IdentityWithTrait Avenger)
  }

asgard :: CardDef
asgard = unique $ identitySupport "06007" "Asgard" 3 [Asgard, Location] [Mental]

hallOfHeroes :: CardDef
hallOfHeroes = deckLimited 1 $ reprintedAs ["25016"] $ unique $ support "06017" "Hall of Heroes" 2 [Asgard, Location] [Mental] Aggression

sanctumSanctorum :: CardDef
sanctumSanctorum =
  unique $ identitySupport "09008" "Sanctum Sanctorum" 1 [Location] [Mental]

theNightNurse :: CardDef
theNightNurse =
  unique $ support "09019" "The Night Nurse" 1 [Persona] [Mental] Protection

teamTraining :: CardDef
teamTraining = reprintedAs ["45013"] $
  limited (MaxPerPlayerUnderAnyControl 1) $
    support "04016" "Team Training" 2 [Condition] [Physical] Leadership

jessicaDrewsApartment :: CardDef
jessicaDrewsApartment = unique $ identitySupport "04034" "Jessica Drew's Apartment" 1 [Location] [Physical]

-- Ant-Man

armyOfAnts :: CardDef
armyOfAnts = identitySupport "12007" "Army of Ants" 1 [Ants, Tiny] [Physical]

teamBuildingExercise :: CardDef
teamBuildingExercise = reprintedAs ["46021", "30022"] $ basicSupport "12024" "Team-Building Exercise" 2 [Condition] [Physical]

-- Wasp

bootCamp :: CardDef
bootCamp =
  limited (MaxPerPlayerUnderAnyControl 1) $
    support "13016" "Boot Camp" 3 [Location] [Mental] Aggression

-- Quicksilver

servalIndustries :: CardDef
servalIndustries = unique $ identitySupport "14007" "Serval Industries" 1 [Location, XFactor] [Mental]

-- Scarlet Witch

agathaHarkness :: CardDef
agathaHarkness = unique $ identitySupport "15007" "Agatha Harkness" 1 [Persona] [Mental]

-- Star-Lord

badBoy :: CardDef
badBoy = unique $ identitySupport "17006" "Bad Boy" 3 [Vehicle] [Physical]

targetPractice :: CardDef
targetPractice = support "17017" "Target Practice" 0 [Skill] [Mental] Leadership

citt :: CardDef
citt = unique $ basicSupport "17021" "C.I.T.T." 2 [Vehicle] [Wild]

knowhere :: CardDef
knowhere =
  deckLimited 1 $ reprintedAs ["22021"] $ (unique $ basicSupport "17022" "Knowhere" 2 [Location] [Energy])
    { cdCriteria = SelfMatches (IdentityWithTrait Guardian)
    }

-- Gamora

conditioningRoom :: CardDef
conditioningRoom = identitySupport "18008" "Conditioning Room" 1 [Location] [Mental]

-- Groot

fertileGround :: CardDef
fertileGround = identitySupport "16007" "Fertile Ground" 1 [Location] [Energy]

-- The Galaxy's Most Wanted

-- | The encounter sets' supports: in play under a player's control as their set says,
-- never in a player's deck.
allEncounterSupports :: HashMap CardCode CardDef
allEncounterSupports = fromList $ map (toFst toCardCode) [milano]

-- | Permanent. Setup. The first player controls the Milano (Ship Command).
milano :: CardDef
milano =
  (unique $ baseSupport "16142" "Milano" 0 [Aerial, Vehicle] [] Nothing)
    { cdCost = Nothing
    , cdKeywords = fromList [Permanent, Setup]
    , cdEncounterSet = Just ShipCommand
    , cdEncounterSetQuantity = Just 1
    }
-- Drax

regroup :: CardDef
regroup = support "19032" "Regroup" 1 [Tactic] [Mental] Leadership

-- Venom

projectRebirth20 :: CardDef
projectRebirth20 = unique $ identitySupport "20007" "Project Rebirth 2.0" 1 [] [Physical]

crewQuarters :: CardDef
crewQuarters = limited (MaxPerPlayerUnderAnyControl 1) $ basicSupport "20029" "Crew Quarters" 1 [Location] [Mental]

-- Spectrum

mightyAvengers :: CardDef
mightyAvengers = limited (MaxTraitPerPlayerUnderAnyControl Team 1) $ support "21015" "Mighty Avengers" 3 [Team] [Energy] Leadership

-- Adam Warlock

soulWorld :: CardDef
soulWorld = unique $ identitySupport "21033" "Soul World" 1 [Location] [Mental]

-- Nebula

nebulasShipSupport :: CardDef
nebulasShipSupport = deckLimited 1 $ identitySupport "22003" "Nebula's Ship" 2 [Vehicle] [Energy]

guardiansOfTheGalaxy :: CardDef
guardiansOfTheGalaxy = deckLimited 3 $ limited (MaxTraitPerPlayerUnderAnyControl Team 1) $ support "22033" "Guardians of the Galaxy" 2 [Team] [Mental] Leadership

defensiveTraining :: CardDef
defensiveTraining = deckLimited 2 $ support "22034" "Defensive Training" 1 [Condition] [Mental] Protection

-- War Machine

munitionsBunker :: CardDef
munitionsBunker = deckLimited 1 $ identitySupport "23003" "Munitions Bunker" 2 [Location, Shield] [Physical]

commandTeam :: CardDef
commandTeam = reprintedAs ["50016"] $ deckLimited 3 $ support "23016" "Command Team" 2 [Shield] [Energy] Leadership

vigilanteTraining :: CardDef
vigilanteTraining = deckLimited 2 $ support "23033" "Vigilante Training" 1 [Condition] [Mental] Justice

-- Valkyrie

valhalla :: CardDef
valhalla = deckLimited 1 $ (identitySupport "25004" "Valhalla" 2 [Asgard, Location] [Mental])
    { cdUnique = True
    }

theBifrost :: CardDef
theBifrost = deckLimited 1 $ (basicSupport "25023" "The Bifrost" 1 [Asgard, Location] [Energy])
    { cdCriteria = SelfMatches (IdentityWithTrait Asgard)
    , cdUnique = True
    }

leadershipTraining :: CardDef
leadershipTraining = deckLimited 2 $ support "25034" "Leadership Training" 1 [Condition] [Mental] Leadership

-- Vision

hickoryBranchLane616 :: CardDef
hickoryBranchLane616 = deckLimited 1 $ (identitySupport "26004" "616 Hickory Branch Lane" 1 [Location] [Mental])
    { cdUnique = True
    }

assaultTraining :: CardDef
assaultTraining = deckLimited 2 $ support "26033" "Assault Training" 1 [Condition] [Mental] Aggression


-- Sinister Motives

georgeStacy :: CardDef
georgeStacy = (identitySupport "27007" "George Stacy" 1 [Persona] [Energy])
    { cdUnique = True
    , cdDeckLimit = Just 1
    }

webOfLifeAndDestiny :: CardDef
webOfLifeAndDestiny = reprintedAs ["30023"] $ (basicSupport "27023" "Web of Life and Destiny" 3 [Location, WebWarrior] [Energy])
    { cdUnique = True
    , cdDeckLimit = Just 1
    }

gankeLee :: CardDef
gankeLee = (identitySupport "27035" "Ganke Lee" 2 [Persona] [Wild])
    { cdUnique = True
    , cdDeckLimit = Just 1
    }

jeffersonDavis :: CardDef
jeffersonDavis = (identitySupport "27036" "Jefferson Davis" 2 [Shield, Persona] [Physical])
    { cdUnique = True
    , cdDeckLimit = Just 1
    }

fieldAgent :: CardDef
fieldAgent = (support "27044" "Field Agent" 1 [Shield, Persona] [Energy] Justice)
    { cdDeckLimit = Just 3
    }

governmentLiaison :: CardDef
governmentLiaison = (basicSupport "27054" "Government Liaison" 2 [Shield, Persona] [Mental])
    { cdDeckLimit = Just 3
    }

skyDestroyer :: CardDef
skyDestroyer = reprintedAs ["50057"] $ (basicSupport "27055" "Sky-Destroyer" 3 [Shield, Vehicle] [Energy])
    { cdUnique = True
    , cdDeckLimit = Just 1
    }

-- Nova

jesseAlexander :: CardDef
jesseAlexander = (identitySupport "28008" "Jesse Alexander" 2 [Persona] [Mental])
  { cdDeckLimit = Just 1
  , cdUnique = True
  }

championsMobileBunker :: CardDef
championsMobileBunker = (basicSupport "28020" "Champions Mobile Bunker" 2 [Champion, Vehicle] [Physical])
  { cdDeckLimit = Just 1
  , cdUnique = True
  }


-- Ironheart

ronnieWilliams :: CardDef
ronnieWilliams = (identitySupport "29010" "Ronnie Williams" 1 [Persona] [Wild])
  { cdDeckLimit = Just 1
  , cdUnique = True
  }

tonyStarkAI :: CardDef
tonyStarkAI = (identitySupport "29011" "Tony Stark A.I." 2 [Persona] [Mental])
  { cdDeckLimit = Just 1
  , cdUnique = True
  }

rAndDFacility :: CardDef
rAndDFacility = (support "29020" "R&D Facility" 3 [Location, Shield] [Mental] Leadership)
  { cdDeckLimit = Just 3
  , cdUnique = False
  , cdKeywords = fromList [SMKeyword.Requirement [Mental, Mental]]
  }


-- Spider-Ham

theDailyBeagle :: CardDef
theDailyBeagle = (identitySupport "30008" "The Daily Beagle" 2 [Cartoon, Location] [Mental])
  { cdDeckLimit = Just 1
  , cdUnique = True
  }


-- SP//dr

spdrSuitInactive :: CardDef
spdrSuitInactive = (identitySupport "31001b" "SP//dr Suit" 0 [Inactive, Tech] [])
  { cdCost = Nothing
  , cdUnique = True
  , cdDeckLimit = Just 1
  , cdKeywords = fromList [SpdrKeyword.Permanent]
  }

auntMayAndUncleBen :: CardDef
auntMayAndUncleBen = (identitySupport "31007" "Aunt May & Uncle Ben" 1 [Persona] [Mental])
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

ejectionProtocol :: CardDef
ejectionProtocol = (identitySupport "31008" "Ejection Protocol" 0 [Tactic] [Physical])
  { cdUnique = False
  , cdDeckLimit = Just 1
  }

spdrCommand :: CardDef
spdrCommand = (identitySupport "31009" "SP//dr Command" 1 [Location] [Mental])
  { cdUnique = False
  , cdDeckLimit = Just 1
  }

-- Mutant Genesis

piotrsStudio :: CardDef
piotrsStudio = (identitySupport "32003" "Piotr's Studio" 1 [Location] [Mental])
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

-- Mutant Genesis

theXjet :: CardDef
theXjet = reprintedAs ["36023"] $ (basicSupport "32020" "The X-Jet" 3 [Vehicle, XMen] [Energy])
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

-- Mutant Genesis

kittysRoom :: CardDef
kittysRoom = (identitySupport "32033" "Kitty's Room" 1 [Location] [Physical])
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

-- Mutant Genesis

xmansion :: CardDef
xmansion = reprintedAs ["37018", "36025"] $ (basicSupport "32049" "X-Mansion" 2 [Location, XMen] [Mental])
  { cdUnique = True
  , cdDeckLimit = Just 1
  }

mgMetroPD :: CardDef
mgMetroPD = (basicSupport "32171b" "Metro P.D." 0 [MGTrait.Persona] [MGResource.Mental]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1, cdKeywords = fromList [MGKeyword.Permanent]}

-- Wolverine

logansCabin :: CardDef
logansCabin = (identitySupport "35007" "Logan's Cabin" 1 [Location] [Mental]) {cdUnique = True, cdDeckLimit = Just 1}

weaponX :: CardDef
weaponX = (basicSupport "35022" "Weapon X" 1 [Location] [Mental]) {cdUnique = True, cdDeckLimit = Just 1, cdCriteria = SelfMatches (IdentityWithTrait Mutant)}

commandCenter :: CardDef
commandCenter = (support "35032" "Command Center" 1 [Location] [Mental] Justice) {cdDeckLimit = Just 3, cdLimit = Just (MaxPerPlayer 1)}


-- Phoenix

whiteHotRoom :: CardDef
whiteHotRoom = (identitySupport "34004" "White Hot Room" 2 [Location, Phoenix] [Mental]) {cdDeckLimit = Just 1, cdUnique = True}

cerebro :: CardDef
cerebro = (basicSupport "34022" "Cerebro" 1 [Location, XMen] [Mental]) {cdDeckLimit = Just 1, cdUnique = True, cdCriteria = SelfMatches (IdentityWithTrait Mutant)}


-- Cyclops

utopia :: CardDef
utopia = reprintedAs ["36024"] $ (basicSupport "33020" "Utopia" 2 [Location, XMen] [Physical]) { cdDeckLimit = Just 1, cdUnique = True }

dangerRoom :: CardDef
dangerRoom = (basicSupport "33021" "Danger Room" 2 [Location, XMen] [Energy]) { cdDeckLimit = Just 1, cdUnique = True }


-- Storm

clearSkies :: CardDef
clearSkies = (identitySupport "36002" "Clear Skies" 0 [Weather] [Wild]) { cdDeckLimit = Just 1, cdCost = Nothing, cdKeywords = fromList [Permanent] }

hurricane :: CardDef
hurricane = (identitySupport "36003" "Hurricane" 0 [Weather] [Physical]) { cdDeckLimit = Just 1, cdCost = Nothing, cdKeywords = fromList [Permanent] }

thunderstorm :: CardDef
thunderstorm = (identitySupport "36004" "Thunderstorm" 0 [Weather] [Energy]) { cdDeckLimit = Just 1, cdCost = Nothing, cdKeywords = fromList [Permanent] }

blizzard :: CardDef
blizzard = (identitySupport "36005" "Blizzard" 0 [Weather] [Mental]) { cdDeckLimit = Just 1, cdCost = Nothing, cdKeywords = fromList [Permanent] }

ororosGarden :: CardDef
ororosGarden = (identitySupport "36008" "Ororo's Garden" 1 [Location] [Physical]) { cdUnique = True, cdDeckLimit = Just 1 }

uncannyXMen :: CardDef
uncannyXMen = (support "36018" "Uncanny X-Men" 3 [Team] [Mental] Leadership) { cdDeckLimit = Just 3, cdLimit = Just (MaxTraitPerPlayerUnderAnyControl Team 1) }

hangarBay :: CardDef
hangarBay = (support "36035" "Hangar Bay" 1 [Location] [Mental] Protection) { cdDeckLimit = Just 3, cdLimit = Just (MaxPerPlayer 1) }

-- Gambit

theThievesGuild :: CardDef
theThievesGuild = (identitySupport "37003" "The Thieves Guild" 1 [Guild] [Mental]) { cdDeckLimit = Just 1, cdUnique = True }

warRoom :: CardDef
warRoom = (support "37030" "War Room" 1 [Location] [Mental] Aggression) { cdDeckLimit = Just 3, cdLimit = Just (MaxPerPlayer 1) }


-- Rogue

moiraMacTaggert :: CardDef
moiraMacTaggert = reprintedAs ["48022"] $ (basicSupport "38018" "Moira MacTaggert" 2 [Persona] [Mental]) { cdUnique = True, cdDeckLimit = Just 1, cdCriteria = SelfMatches (IdentityWithTrait Mutant) }

medLab :: CardDef
medLab = (support "38028" "Med Lab" 1 [Location] [Mental] Leadership) { cdLimit = Just (MaxPerPlayer 1), cdDeckLimit = Just 3 }

-- NeXt Evolution: Cable

cable40007 :: CardDef
cable40007 = (identitySupport "40007" "Graymalkin" 2 [NE.Location] [NE.Energy]) { cdUnique = True, cdDeckLimit = Just 1 }

cable40008 :: CardDef
cable40008 = (identitySupport "40008" "Professor" 1 [NE.Persona, NE.Tech] [NE.Mental]) { cdUnique = True, cdDeckLimit = Just 1 }

cable40021 :: CardDef
cable40021 = (support "40021" "E.V.A." 0 [NE.Vehicle] [NE.Wild] NE.Leadership) { cdUnique = True, cdDeckLimit = Just 1 }

cable40022 :: CardDef
cable40022 = (support "40022" "Uncanny X-Force" 2 [NE.Team] [NE.Energy] NE.Leadership) { cdUnique = False, cdDeckLimit = Just 3 , cdLimit = Just (MaxTraitPerPlayerUnderAnyControl NE.Team 1) }

-- NeXt Evolution: Domino
pipthePug :: CardDef
pipthePug = CardDef
  { cdCardCode = "40044", cdName = "Pip the Pug", cdCost = Just 1
  , cdTraits = fromList [NET.Creature], cdKeywords = fromList []
  , cdCardType = SupportType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
thePaintedLady :: CardDef
thePaintedLady = CardDef
  { cdCardCode = "40045", cdName = "The Painted Lady", cdCost = Just 1
  , cdTraits = fromList [NET.Vehicle], cdKeywords = fromList []
  , cdCardType = SupportType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution campaign and scenarios

ne40197 :: CardDef
ne40197 = CardDef
  { cdCardCode = "40197"
  , cdName = "Safehouse"
  , cdCost = Just 4
  , cdTraits = fromList [NETrait.Location]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NEResource.Wild)]
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

-- Psylocke

trainingRegimen :: CardDef
trainingRegimen = CardDef
  { cdCardCode = "41008", cdName = "Training Regimen", cdCost = Just 1
  , cdTraits = fromList [PsyTrait.Training], cdKeywords = fromList []
  , cdCardType = PsyCard.SupportType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Physical)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

ipac :: CardDef
ipac = reprintedAs ["43025"] $ CardDef
  { cdCardCode = "41022", cdName = "IPAC", cdCost = Just 1
  , cdTraits = fromList [PsyTrait.Vehicle, PsyTrait.XForce], cdKeywords = fromList []
  , cdCardType = PsyCard.SupportType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just PsyAspect.Neutral, cdCriteria = PsyCriteria.SelfMatches (PsyMatch.IdentityWithTrait PsyTrait.XForce)
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Mental)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

xBunker :: CardDef
xBunker = reprintedAs ["43026"] $ CardDef
  { cdCardCode = "41023", cdName = "X-Bunker", cdCost = Just 2
  , cdTraits = fromList [PsyTrait.Location, PsyTrait.XForce], cdKeywords = fromList []
  , cdCardType = PsyCard.SupportType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Just PsyAspect.Neutral, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [(PsyCard.PrintedResource, PsyResource.Energy)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

-- Angel

worthingtonIndustries :: CardDef
worthingtonIndustries = CardDef
  { cdCardCode = "42009"
  , cdName = "Worthington Industries"
  , cdCost = Just 1
  , cdTraits = fromList [AnTrait.Location]
  , cdKeywords = fromList []
  , cdCardType = AnCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
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

angelsAerie :: CardDef
angelsAerie = CardDef
  { cdCardCode = "42018"
  , cdName = "Angel's Aerie"
  , cdCost = Just 1
  , cdTraits = fromList [AnTrait.Location]
  , cdKeywords = fromList []
  , cdCardType = AnCard.SupportType
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

-- X-23

sisterhood :: CardDef
sisterhood = CardDef
  { cdCardCode = "43008"
  , cdName = "Sisterhood"
  , cdCost = Just 2
  , cdTraits = fromList [AnTrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AnCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
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

chimichangaTruck :: CardDef
chimichangaTruck = DpCard.CardDef
  { cdCardCode = "44008"
  , cdName = DpName.Name "Chimichanga Truck" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList [DpTrait.Location]
  , cdKeywords = fromList []
  , cdCardType = DpCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Mental)]
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
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

deadpoolCorpsShip :: CardDef
deadpoolCorpsShip = DpCard.CardDef
  { cdCardCode = "44049"
  , cdName = DpName.Name "Deadpool Corps Ship" (Nothing)
  , cdCost = Just 1
  , cdTraits = fromList [DpTrait.Vehicle]
  , cdKeywords = fromList []
  , cdCardType = DpCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Mental)]
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
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

plotConvenience :: CardDef
plotConvenience = DpCard.CardDef
  { cdCardCode = "44050"
  , cdName = DpName.Name "Plot Convenience" (Nothing)
  , cdCost = Just 2
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Mental)]
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
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Age of Apocalypse: Magik
limbo :: CardDef
limbo = CardDef
  { cdCardCode = "45032"
  , cdName = "Limbo"
  , cdCost = Just 1
  , cdTraits = fromList [AoaMT.Dimension]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, AoaMR.Physical)]
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
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse: Magik
stepfordCuckoos :: CardDef
stepfordCuckoos = CardDef
  { cdCardCode = "45049"
  , cdName = "Stepford Cuckoos"
  , cdCost = Just 3
  , cdTraits = fromList [AoaMT.Persona, AoaMT.Psionic]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AoaMA.Neutral
  , cdCriteria = SelfMatches (AoaMM.IdentityWithTrait AoaMT.XMen)
  , cdResources = [(PrintedResource, AoaMR.Mental)]
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
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse campaign and scenarios

missionTeamFront :: CardDef
missionTeamFront = CardDef
  { cdCardCode = "45171a"
  , cdName = "Mission Team"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mission]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just NEAspect.Neutral
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NEResource.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaBasicCampaign
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

missionTeamFinished :: CardDef
missionTeamFinished = CardDef
  { cdCardCode = "45171b"
  , cdName = "Mission Team"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Finished]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just NEAspect.Neutral
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NEResource.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaBasicCampaign
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

-- Magneto

asteroidM :: CardDef
asteroidM = CardDef
  { cdCardCode = "49002"
  , cdName = "Asteroid M"
  , cdCost = Just 2
  , cdTraits = fromList [MgTrait.Location, MgTrait.Magnetic]
  , cdKeywords = fromList []
  , cdCardType = MgCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = [(MgCard.PrintedResource, MgResource.Wild)]
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

wontStayDown :: CardDef
wontStayDown = CardDef
  { cdCardCode = "49016"
  , cdName = "Won't Stay Down"
  , cdCost = Just 1
  , cdTraits = fromList [MgTrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = MgCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just MgAspect.Leadership
  , cdCriteria = MgCriteria.AnyCriteria [MgCriteria.SelfMatches (MgMatch.IdentityWithTrait MgTrait.XForce), MgCriteria.SelfMatches (MgMatch.IdentityWithTrait MgTrait.XMen)]
  , cdResources = [(MgCard.PrintedResource, MgResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Just (MgCard.MaxPerPlayer 1)
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

childrenOfTheAtom :: CardDef
childrenOfTheAtom = CardDef
  { cdCardCode = "49037"
  , cdName = "Children of the Atom"
  , cdCost = Just 1
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = MgCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just MgAspect.Neutral
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = [(MgCard.PrintedResource, MgResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Just (MgCard.MaxPerPlayerUnderAnyControl 1)
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

-- Iceman
iceWall :: CardDef
iceWall = IcemanPack.iceWall

-- Nightcrawler
nc48003 :: CardDef
nc48003 = CardDef
  { cdCardCode = "48003"
  , cdName = "Kurt's Chapel"
  , cdCost = Just 1
  , cdTraits = fromList [NcTrait.Location]
  , cdKeywords = fromList []
  , cdCardType = SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = [(PrintedResource, NcResource.Mental)]
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

-- Agents of Shield players

aosP50008 :: CardDef
aosP50008 = CardDef
  { cdCardCode = "50008"
  , cdName = "Support Staff"
  , cdCost = Just 1
  , cdTraits = fromList [AospTrait.Persona, AospTrait.Shield]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Mental)]
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
  , cdAbilityCost = AospCost.NoCost
  }

aosP50009 :: CardDef
aosP50009 = CardDef
  { cdCardCode = "50009"
  , cdName = "The Iliad"
  , cdCost = Just 6
  , cdTraits = fromList [AospTrait.Shield, AospTrait.Vehicle]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Energy)]
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
  , cdAbilityCost = AospCost.NoCost
  }

aosP50015 :: CardDef
aosP50015 = CardDef
  { cdCardCode = "50015"
  , cdName = "Agents of S.H.I.E.L.D."
  , cdCost = Just 3
  , cdTraits = fromList [AospTrait.Team]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just AospAspect.Leadership
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Just (MaxTraitPerPlayerUnderAnyControl AospTrait.Team 1)
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }

aosP50017 :: CardDef
aosP50017 = CardDef
  { cdCardCode = "50017"
  , cdName = "The Circe"
  , cdCost = Just 6
  , cdTraits = fromList [AospTrait.Shield, AospTrait.Vehicle]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AospAspect.Leadership
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Physical)]
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
  , cdAbilityCost = AospCost.NoCost
  }

aosP50018 :: CardDef
aosP50018 = CardDef
  { cdCardCode = "50018"
  , cdName = "The Bellerophon"
  , cdCost = Just 6
  , cdTraits = fromList [AospTrait.Shield, AospTrait.Vehicle]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AospAspect.Aggression
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Energy)]
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
  , cdAbilityCost = AospCost.NoCost
  }

aosP50019 :: CardDef
aosP50019 = CardDef
  { cdCardCode = "50019"
  , cdName = "The Douglass"
  , cdCost = Just 6
  , cdTraits = fromList [AospTrait.Shield, AospTrait.Vehicle]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AospAspect.Justice
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Mental)]
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
  , cdAbilityCost = AospCost.NoCost
  }

aosP50020 :: CardDef
aosP50020 = CardDef
  { cdCardCode = "50020"
  , cdName = "The Pericles"
  , cdCost = Just 6
  , cdTraits = fromList [AospTrait.Shield, AospTrait.Vehicle]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AospAspect.Protection
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Energy)]
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
  , cdAbilityCost = AospCost.NoCost
  }

aosP50028 :: CardDef
aosP50028 = CardDef
  { cdCardCode = "50028"
  , cdName = "Front Organization"
  , cdCost = Just 0
  , cdTraits = fromList [AospTrait.Location]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just AospAspect.Neutral
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Just (MaxPerPlayerUnderAnyControl 1)
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }

aosP50040 :: CardDef
aosP50040 = CardDef
  { cdCardCode = "50040"
  , cdName = "Fury's Flying Car"
  , cdCost = Just 2
  , cdTraits = fromList [AospTrait.Aerial, AospTrait.Tech, AospTrait.Vehicle]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Energy)]
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
  , cdAbilityCost = AospCost.NoCost
  }

aosP50041 :: CardDef
aosP50041 = CardDef
  { cdCardCode = "50041"
  , cdName = "Safe House #221"
  , cdCost = Just 1
  , cdTraits = fromList [AospTrait.Location, AospTrait.Shield]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Mental)]
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
  , cdAbilityCost = AospCost.NoCost
  }

aosP50055 :: CardDef
aosP50055 = CardDef
  { cdCardCode = "50055"
  , cdName = "Jemma Simmons"
  , cdCost = Just 3
  , cdTraits = fromList [AospTrait.Persona, AospTrait.Shield]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AospAspect.Neutral
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Mental)]
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
  , cdAbilityCost = AospCost.NoCost
  }

aosP50056 :: CardDef
aosP50056 = CardDef
  { cdCardCode = "50056"
  , cdName = "Leo Fitz"
  , cdCost = Just 3
  , cdTraits = fromList [AospTrait.Persona, AospTrait.Shield]
  , cdKeywords = fromList []
  , cdCardType = AospCard.SupportType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Just AospAspect.Neutral
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Mental)]
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
  , cdAbilityCost = AospCost.NoCost
  }
