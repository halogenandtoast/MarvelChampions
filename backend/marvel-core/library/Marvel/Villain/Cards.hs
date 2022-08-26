module Marvel.Villain.Cards where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Trait

allVillainCards :: [CardDef]
allVillainCards = [rhino1, rhino2, rhino3, klaw1, klaw2, klaw3]

allVillainsMap :: HashMap CardCode CardDef
allVillainsMap = fromList $ map (toFst toCardCode) allVillainCards

villain :: CardCode -> Name -> [Trait] -> CardDef
villain cardCode name traits = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubType = Nothing
  , cdUnique = True
  , cdAspect = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdResponseWindow = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdLimit = Nothing
  }

rhino1 :: CardDef
rhino1 = villain "01094" "Rhino" [Brute, Criminal]

rhino2 :: CardDef
rhino2 = villain "01095" "Rhino" [Brute, Criminal]

rhino3 :: CardDef
rhino3 = villain "01096" "Rhino" [Brute, Criminal]

klaw1 :: CardDef
klaw1 = villain "01113" "Klaw" [MastersOfEvil]

klaw2 :: CardDef
klaw2 = villain "01114" "Klaw" [MastersOfEvil]

klaw3 :: CardDef
klaw3 = villain "01115" "Klaw" [MastersOfEvil]
