module Marvel.MainScheme.Cards where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Name

allMainSchemes :: HashMap CardCode CardDef
allMainSchemes =
  fromList $ map (toFst toCardCode) [theBreakIn, undergroundDistribution]

mainScheme :: CardCode -> Name -> EncounterSet -> CardDef
mainScheme code name encounterSet = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubType = Nothing
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just encounterSet
  , cdEncounterSetQuantity = Just 1
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdResponseWindow = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdLimit = Nothing
  }

theBreakIn :: CardDef
theBreakIn = mainScheme "01097" "Breakin' & Takin'" Rhino

undergroundDistribution :: CardDef
undergroundDistribution = mainScheme "01116" "Underground Distribution" Klaw

secretRendezvous :: CardDef
secretRendezvous = mainScheme "01117" "Secret Rendezvous" Klaw
