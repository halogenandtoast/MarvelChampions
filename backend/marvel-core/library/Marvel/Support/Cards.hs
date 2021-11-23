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
allSupports = fromList $ map (toCardCode &&& id) [auntMay, helicarrier]

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
  , cdUnique = False
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = map (PrintedResource, ) resources
  , cdResponseWindow = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  }

unique :: CardDef -> CardDef
unique def = def { cdUnique = True }

auntMay :: CardDef
auntMay = unique $ identitySupport "01006" "Aunt May" 1 [Persona] [Energy]

helicarrier :: CardDef
helicarrier =
  basicSupport "01092" "Helicarrier" 3 [Location, Shield] [Physical]
