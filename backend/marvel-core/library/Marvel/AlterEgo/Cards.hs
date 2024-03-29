module Marvel.AlterEgo.Cards where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Trait

allAlterEgoCards :: [CardDef]
allAlterEgoCards =
  [peterParker, carolDanvers, jenniferWalters, tonyStark, tChalla]

allAlterEgosMap :: HashMap CardCode CardDef
allAlterEgosMap = fromList $ map (toFst toCardCode) allAlterEgoCards

alterEgo :: CardCode -> Name -> [Trait] -> CardDef
alterEgo cardCode name traits = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = AlterEgoType
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

peterParker :: CardDef
peterParker = alterEgo "01001b" "Peter Parker" [Genius]

carolDanvers :: CardDef
carolDanvers = alterEgo "01010b" "Carol Danvers" [Shield, Soldier]

jenniferWalters :: CardDef
jenniferWalters = alterEgo "01019b" "Jennifer Walters" [Attorney, Gamma]

tonyStark :: CardDef
tonyStark = alterEgo "01029b" "Tony Stark" [Genius]

tChalla :: CardDef
tChalla = alterEgo "01040b" "T'Challa" [King, Wakanda]
