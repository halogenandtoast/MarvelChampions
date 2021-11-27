module Marvel.SideScheme.Cards where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Name

allSideSchemes :: HashMap CardCode CardDef
allSideSchemes = fromList $ map
  (toCardCode &&& id)
  [breakinAndTakin, crowdControl, bombScare, highwayRobbery]

sideScheme
  :: CardCode -> Name -> [BoostIcon] -> EncounterSet -> Natural -> CardDef
sideScheme code name boostIcons encounterSet quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = SideSchemeType
  , cdAbilityType = Nothing
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just encounterSet
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdResponseWindow = Nothing
  , cdBoostIcons = boostIcons
  , cdHazards = 0
  , cdAcceleration = 0
  }

breakinAndTakin :: CardDef
breakinAndTakin =
  (sideScheme "01107" "Breakin' & Takin'" [Boost, Boost] Rhino 1)
    { cdHazards = 1
    }

crowdControl :: CardDef
crowdControl = sideScheme "01108" "Crowd Control" [Boost, Boost] Rhino 1

bombScare :: CardDef
bombScare = (sideScheme "01109" "Bomb Scare" [Boost, Boost] BombScare 1)
  { cdAcceleration = 1
  }

highwayRobbery :: CardDef
highwayRobbery =
  (sideScheme "01166" "Highway Robbery" [Boost, Boost, Boost] SpiderManNemesis 1
    )
    { cdAcceleration = 1
    }
