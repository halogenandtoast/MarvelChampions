module Marvel.Support.Cards where

import Marvel.Keyword qualified as SMKeyword
import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Resource
import Marvel.Trait
import Marvel.Matchers
import Marvel.EncounterSet (EncounterSet (ShipCommand))
import Marvel.Keyword (Keyword (Permanent, Setup))

allSupports :: HashMap CardCode CardDef
allSupports = fromList $ concatMap
  toCardCodePairs
  [ alphaFlightStation
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
theTriskellion = unique
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
teamTraining =
  limited (MaxPerPlayerUnderAnyControl 1) $
    support "04016" "Team Training" 2 [Condition] [Physical] Leadership

jessicaDrewsApartment :: CardDef
jessicaDrewsApartment = unique $ identitySupport "04034" "Jessica Drew's Apartment" 1 [Location] [Physical]

-- Ant-Man

armyOfAnts :: CardDef
armyOfAnts = identitySupport "12007" "Army of Ants" 1 [Ants, Tiny] [Physical]

teamBuildingExercise :: CardDef
teamBuildingExercise = reprintedAs ["30022"] $ basicSupport "12024" "Team-Building Exercise" 2 [Condition] [Physical]

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
commandTeam = deckLimited 3 $ support "23016" "Command Team" 2 [Shield] [Energy] Leadership

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
skyDestroyer = (basicSupport "27055" "Sky-Destroyer" 3 [Shield, Vehicle] [Energy])
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
