
module Marvel.Obligation.Cards where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name

allObligations :: HashMap CardCode CardDef
allObligations = fromList $ map (toCardCode &&& id) [legalWork, evictionNotice]

obligation :: CardCode -> Name -> [BoostIcon] -> CardDef
obligation code name boostIcons = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = ObligationType
  , cdAbilityType = Nothing
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdResponseWindow = Nothing
  , cdBoostIcons = boostIcons
  , cdHazards = 0
  , cdAcceleration = 0
  }

legalWork :: CardDef
legalWork = obligation "01160" "Legal Work" [Boost, Boost]

evictionNotice :: CardDef
evictionNotice = obligation "01165" "Eviction Notice" [Boost, Boost]
