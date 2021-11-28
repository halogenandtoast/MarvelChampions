module Marvel.Treachery.Cards where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Name

allTreacheries :: HashMap CardCode CardDef
allTreacheries = fromList $ map
  (toCardCode &&& id)
  [ hardToKeepDown
  , imTough
  , stampede
  , explosion
  , falseAlarm
  , heartShapedHerb
  , ritualCombat
  , sweepingSwoop
  , theVulturesPlans
  , advance
  , assault
  , caughtOffGuard
  , gangUp
  , shadowOfThePast
  ]

treachery
  :: CardCode -> Name -> [BoostIcon] -> EncounterSet -> Natural -> CardDef
treachery code name boostIcons encounterSet quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = TreacheryType
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

hardToKeepDown :: CardDef
hardToKeepDown = treachery "01104" "Hard to Keep Down" [] Rhino 2

imTough :: CardDef
imTough = treachery "01105" "\"I'm Tough!\"" [] Rhino 2

stampede :: CardDef
stampede = treachery "01106" "Stampede" [Boost] Rhino 3

explosion :: CardDef
explosion = treachery "01111" "Explosion" [Boost, Boost] BombScare 1

falseAlarm :: CardDef
falseAlarm = treachery "01112" "False Alarm" [Boost] BombScare 2

heartShapedHerb :: CardDef
heartShapedHerb =
  treachery "01158" "Heart-Shaped Herb" [Star] BlackPantherNemesis 1

ritualCombat :: CardDef
ritualCombat =
  treachery "01159" "Ritual Combat" [Boost, Boost] BlackPantherNemesis 2

titaniasFury :: CardDef
titaniasFury =
  treachery "01164" "Titania's Fury" [Star, Boost] SheHulkNemesis 2

sweepingSwoop :: CardDef
sweepingSwoop = treachery "01168" "Sweeping Shoop" [Star] SpiderManNemesis 2

theVulturesPlans :: CardDef
theVulturesPlans =
  treachery "01169" "The Vulture's Plans" [Boost, Boost] SpiderManNemesis 1

advance :: CardDef
advance = treachery "01186" "Advance" [] Standard 2

assault :: CardDef
assault = treachery "01187" "Assault" [] Standard 2

caughtOffGuard :: CardDef
caughtOffGuard = treachery "01188" "Caught Off Guard" [Boost] Standard 1

gangUp :: CardDef
gangUp = treachery "01189" "Gang-Up" [Boost] Standard 1

shadowOfThePast :: CardDef
shadowOfThePast =
  treachery "01190" "Shadow of the Past" [Boost, Boost] Standard 1
