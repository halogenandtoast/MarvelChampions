module Marvel.Support.Cards where

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
  baseSupport code name cost traits resources Nothing

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
  , cdAbilitySubType = Nothing
  , cdUnique = False
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = map (PrintedResource, ) resources
  , cdResponseWindow = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdLimit = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
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
tacTeam = support "01056" "Tac Team" 3 [Shield] [Energy] Aggression

interrogationRoom :: CardDef
interrogationRoom = reprintedAs ["08015"] $ limited (MaxPerPlayer 1)
  $ support "01063" "Interrogation Room" 1 [Location] [Energy] Justice

surveillanceTeam :: CardDef
surveillanceTeam = reprintedAs ["08016"] $
  support "01064" "Surveillance Team" 2 [Shield] [Mental] Justice

theTriskellion :: CardDef
theTriskellion = unique
  $ support "01073" "The Triskelion" 1 [Location, Shield] [Energy] Leadership

medTeam :: CardDef
medTeam = reprintedAs ["09018"] $ support "01080" "Med Team" 3 [Shield] [Energy] Protection

avengersMansion :: CardDef
avengersMansion = reprintedAs ["05022", "06025", "09025", "10023"] $ limited (MaxPerPlayer 1)
  $ basicSupport "01091" "Avengers Mansion" 4 [Avenger, Location] [Mental]

helicarrier :: CardDef
helicarrier = reprintedAs ["10024"] $ limited (MaxPerPlayer 1)
  $ basicSupport "01092" "Helicarrier" 3 [Location, Shield] [Physical]

stevesApartment :: CardDef
stevesApartment =
  unique $ identitySupport "03007" "Steve's Apartment" 1 [Location] [Energy]

quinjet :: CardDef
quinjet = support "03019" "Quinjet" 1 [Avenger, Vehicle] [Energy] Leadership

avengersTower :: CardDef
avengersTower =
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
quincarrier = (unique $ basicSupport "08023" "Quincarrier" 3 [Avenger, Vehicle] [Mental])
  { cdCriteria = SelfMatches (IdentityWithTrait Avenger)
  }

asgard :: CardDef
asgard = unique $ identitySupport "06007" "Asgard" 3 [Asgard, Location] [Mental]

hallOfHeroes :: CardDef
hallOfHeroes = unique $ support "06017" "Hall of Heroes" 2 [Asgard, Location] [Mental] Aggression

sanctumSanctorum :: CardDef
sanctumSanctorum =
  unique $ identitySupport "09008" "Sanctum Sanctorum" 1 [Location] [Mental]

theNightNurse :: CardDef
theNightNurse =
  unique $ support "09019" "The Night Nurse" 1 [Persona] [Mental] Protection
