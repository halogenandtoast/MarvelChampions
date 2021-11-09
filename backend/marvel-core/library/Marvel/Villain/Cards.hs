module Marvel.Villain.Cards where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Trait

allVillainCards :: [CardDef]
allVillainCards = [rhino, klaw]

allVillainsMap :: HashMap CardCode CardDef
allVillainsMap = fromList $ map (toCardCode &&& id) allVillainCards

villain :: CardCode -> Name -> [Trait] -> CardDef
villain cardCode name traits = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdCardType = VillainType
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  }

rhino :: CardDef
rhino = villain "01094" "Rhino" [Brute, Criminal]

klaw :: CardDef
klaw = villain "01113" "Klaw" [MastersOfEvil]
