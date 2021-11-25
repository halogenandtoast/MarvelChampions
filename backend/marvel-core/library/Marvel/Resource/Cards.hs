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
  , cdAbilityType = Nothing
  , cdUnique = False
  , cdCriteria = NoCriteria
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResources = map (PrintedResource, ) resources
  , cdResponseWindow = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  }

thePowerOfAggression :: CardDef
thePowerOfAggression =
  resource "01055" "The Power of Aggression" [] [Wild] Aggression
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Aggression, Wild)]

thePowerOfJustice :: CardDef
thePowerOfJustice =
  resource "01062" "The Power of Justice" [] [Wild] Justice
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Justice, Wild)]

thePowerOfLeadership :: CardDef
thePowerOfLeadership =
  resource "01072" "The Power of Leadership" [] [Wild] Leadership
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Leadership, Wild)]

thePowerOfProtection :: CardDef
thePowerOfProtection =
  resource "01079" "The Power of Protection" [] [Wild] Protection
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Protection, Wild)]

energy :: CardDef
energy = basicResource "01088" "Energy" [] [Energy, Energy]

genius :: CardDef
genius = basicResource "01089" "Genius" [] [Mental, Mental]

strength :: CardDef
strength = basicResource "01090" "Strength" [] [Physical, Physical]
