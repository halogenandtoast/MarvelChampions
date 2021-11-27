module Marvel.Minion.Cards where

import Marvel.Prelude

import Data.HashSet (singleton)
import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Keyword
import Marvel.Name
import Marvel.Trait

allMinions :: HashMap CardCode CardDef
allMinions = fromList $ map
  (toCardCode &&& id)
  [hydraMercenary, sandman, shocker, hydraBomber, titania, vulture]

minion
  :: CardCode
  -> Name
  -> [Trait]
  -> [BoostIcon]
  -> EncounterSet
  -> Natural
  -> CardDef
minion code name traits boostIcons encounterSet quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubType = Nothing
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

hydraMercenary :: CardDef
hydraMercenary = (minion "01101" "Hydra Mercenary" [Hydra] [Boost] Rhino 2)
  { cdKeywords = singleton Guard
  }

sandman :: CardDef
sandman = (minion "01102" "Sandman" [Criminal, Elite] [Boost, Boost] Rhino 1)
  { cdKeywords = singleton Toughness
  , cdUnique = True
  }

shocker :: CardDef
shocker = (minion "01103" "Shocker" [Criminal] [Boost, Boost] Rhino 1)
  { cdUnique = True
  }

hydraBomber :: CardDef
hydraBomber = minion "01110" "Hydra Bomber" [Hydra] [Boost] BombScare 2

titania :: CardDef
titania =
  (minion "01162" "Titania" [Brute, Elite] [Boost, Boost] SheHulkNemesis 1)
    { cdUnique = True
    , cdKeywords = singleton Quickstrike
    }

vulture :: CardDef
vulture =
  (minion "01167" "Vulture" [Criminal] [Boost, Boost] SpiderManNemesis 1)
    { cdUnique = True
    , cdKeywords = singleton Quickstrike
    }

