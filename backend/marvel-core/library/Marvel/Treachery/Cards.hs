module Marvel.Treachery.Cards where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Name

allTreacheries :: HashMap CardCode CardDef
allTreacheries =
  fromList $ map (toCardCode &&& id) [hardToKeepDown, imTough, stampede]

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
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just encounterSet
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = boostIcons
  }

hardToKeepDown :: CardDef
hardToKeepDown = treachery "01104" "Hard to Keep Down" [] Rhino 2

imTough :: CardDef
imTough = treachery "01105" "\"I'm Tough!\"" [] Rhino 2

stampede :: CardDef
stampede = treachery "01106" "Stampede" [Boost] Rhino 3
