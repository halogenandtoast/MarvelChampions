module Marvel.Resource.Cards where

import Marvel.Prelude

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Resource
import Marvel.Trait

allResources :: HashMap CardCode CardDef
allResources = fromList
  $ map (toCardCode &&& id) [thePowerOfLeadership, energy, genius, strength]

resource :: CardCode -> Name -> [Trait] -> [Resource] -> Aspect -> CardDef
resource code name traits resources aspect =
  baseResource code name traits resources (Just aspect)

identityResource :: CardCode -> Name -> [Trait] -> [Resource] -> CardDef
identityResource code name traits resources =
  baseResource code name traits resources Nothing

basicResource :: CardCode -> Name -> [Trait] -> [Resource] -> CardDef
basicResource code name traits resources =
  baseResource code name traits resources Nothing

baseResource
  :: CardCode -> Name -> [Trait] -> [Resource] -> Maybe Aspect -> CardDef
baseResource code name traits resources mAspect = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = ResourceType
  , cdUnique = False
  , cdCriteria = NoCriteria
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResources = map (PrintedResource, ) resources
  , cdBoostIcons = []
  }

thePowerOfLeadership :: CardDef
thePowerOfLeadership =
  resource "01072" "The Power of Leadership" [] [Wild] Leadership
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Leadership, Wild)]

energy :: CardDef
energy = basicResource "01088" "Energy" [] [Energy, Energy]

genius :: CardDef
genius = basicResource "01089" "Genius" [] [Mental, Mental]

strength :: CardDef
strength = basicResource "01090" "Strength" [] [Physical, Physical]
