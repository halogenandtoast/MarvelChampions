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
import Marvel.Trait hiding (MastersOfEvil)
import Marvel.Trait qualified as Trait

allMinions :: HashMap CardCode CardDef
allMinions =
  fromList $
    map
      (toCardCode &&& id)
      [ hydraMercenary
      , sandman
      , shocker
      , hydraBomber
      , armoredGuard
      , weaponsRunner
      , radioactiveMan
      , whirlwind
      , tigerShark
      , killmonger
      , titania
      , vulture
      , whiplash
      ]

minion ::
  CardCode ->
  Name ->
  [Trait] ->
  [BoostIcon] ->
  EncounterSet ->
  Natural ->
  CardDef
minion code name traits boostIcons encounterSet quantity =
  CardDef
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

unique :: CardDef -> CardDef
unique def = def {cdUnique = True}

hydraMercenary :: CardDef
hydraMercenary =
  (minion "01101" "Hydra Mercenary" [Hydra] [Boost] Rhino 2)
    { cdKeywords = singleton Guard
    }

sandman :: CardDef
sandman =
  unique $
    (minion "01102" "Sandman" [Criminal, Elite] [Boost, Boost] Rhino 1)
      { cdKeywords = singleton Toughness
      }

shocker :: CardDef
shocker = unique $ minion "01103" "Shocker" [Criminal] [Boost, Boost] Rhino 1

hydraBomber :: CardDef
hydraBomber = minion "01110" "Hydra Bomber" [Hydra] [Boost] BombScare 2

armoredGuard :: CardDef
armoredGuard =
  (minion "01120" "Armored Guard" [Mercenary] [Boost] Klaw 3)
    { cdKeywords = fromList [Guard, Toughness]
    }

weaponsRunner :: CardDef
weaponsRunner =
  (minion "01121" "Weapons Runner" [Mercenary] [Star] Klaw 2)
    { cdKeywords = singleton Surge
    }

radioactiveMan :: CardDef
radioactiveMan =
  unique $ minion "01129" "Radioactive Man" [Elite, Trait.MastersOfEvil] [Star] MastersOfEvil 1

whirlwind :: CardDef
whirlwind =
  unique $ minion "01130" "Whirlwind" [Trait.MastersOfEvil] [Star] MastersOfEvil 1

tigerShark :: CardDef
tigerShark =
  unique $ minion "01131" "Tiger Shark" [Trait.MastersOfEvil] [Star] MastersOfEvil 1

killmonger :: CardDef
killmonger =
  unique $
    minion
      "01157"
      "Killmonger"
      [Assassin, Elite, Mercenary]
      [Boost, Boost]
      BlackPantherNemesis
      1

titania :: CardDef
titania =
  unique $
    (minion "01162" "Titania" [Brute, Elite] [Boost, Boost] SheHulkNemesis 1)
      { cdKeywords = singleton Quickstrike
      }

vulture :: CardDef
vulture =
  unique $
    (minion "01167" "Vulture" [Criminal] [Boost, Boost] SpiderManNemesis 1)
      { cdKeywords = singleton Quickstrike
      }

whiplash :: CardDef
whiplash =
  unique $
    ( minion "01172" "Whiplash" [Criminal] [Boost, Boost] IronManNemesis 1
    )
      { cdKeywords = singleton (Retaliate 1)
      }
