module Marvel.Support.Cards where

import Marvel.Prelude

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Resource
import Marvel.Trait

allSupports :: HashMap CardCode CardDef
allSupports =
  fromList $
    map
      (toCardCode &&& id)
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
      ]

support ::
  CardCode -> Name -> Int -> [Trait] -> [Resource] -> Aspect -> CardDef
support code name cost traits resources aspect =
  baseSupport code name cost traits resources (Just aspect)

identitySupport :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
identitySupport code name cost traits resources =
  baseSupport code name cost traits resources Nothing

basicSupport :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
basicSupport code name cost traits resources =
  baseSupport code name cost traits resources Nothing

baseSupport ::
  CardCode -> Name -> Int -> [Trait] -> [Resource] -> Maybe Aspect -> CardDef
baseSupport code name cost traits resources mAspect =
  CardDef
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
    , cdResources = map (PrintedResource,) resources
    , cdResponseWindow = Nothing
    , cdBoostIcons = []
    , cdHazards = 0
    , cdAcceleration = 0
    }

unique :: CardDef -> CardDef
unique def = def {cdUnique = True}

auntMay :: CardDef
auntMay = unique $ identitySupport "01006" "Aunt May" 1 [Persona] [Energy]

alphaFlightStation :: CardDef
alphaFlightStation = unique $ identitySupport "01015" "Alpha Flight Station" 1 [Location, Shield] [Mental]

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
theGoldenCity =
  unique $ identitySupport "01045" "The Golden City" 2 [Location, Wakanda] [Energy]

tacTeam :: CardDef
tacTeam =
  support "01056" "Tac Team" 3 [Shield] [Energy] Aggression

interrogationRoom :: CardDef
interrogationRoom =
  support "01063" "Interrogation Room" 1 [Location] [Energy] Justice

surveillanceTeam :: CardDef
surveillanceTeam =
  support "01064" "Surveillance Team" 2 [Shield] [Mental] Justice

theTriskellion :: CardDef
theTriskellion =
  unique $ support "01073" "The Triskellion" 3 [Location, Shield] [Energy] Leadership

medTeam :: CardDef
medTeam =
  support "01080" "Med Team" 3 [Shield] [Energy] Protection

avengersMansion :: CardDef
avengersMansion =
  basicSupport "01091" "Avengers Mansion" 4 [Avenger, Location] [Mental]

helicarrier :: CardDef
helicarrier =
  basicSupport "01092" "Helicarrier" 3 [Location, Shield] [Physical]
