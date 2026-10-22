module Marvel.Villain.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Trait
import Data.HashSet (singleton)
import Marvel.Keyword

allVillainCards :: [CardDef]
allVillainCards =
  [ rhino1
  , rhino2
  , rhino3
  , klaw1
  , klaw2
  , klaw3
  , ultron1
  , ultron2
  , ultron3
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  -- Zola
  , zola1
  , zola2
  , zola3

  -- The Once and Future Kang

  ]

allVillainsMap :: HashMap CardCode CardDef
allVillainsMap = fromList $ map (toFst toCardCode) allVillainCards

villain :: CardCode -> Name -> [Trait] -> CardDef
villain cardCode name traits =
  CardDef
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
    , cdAlternateCardCodes = []
    , cdPlayableFromDiscard = False
    , cdAbilityCost = NoCost
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

ultron1 :: CardDef
ultron1 = villain "01134" "Ultron" [Android]

ultron2 :: CardDef
ultron2 = villain "01135" "Ultron" [Android]

ultron3 :: CardDef
ultron3 = (villain "01136" "Ultron" [Android]) {cdHazards = 1}

-- The Rise of Red Skull
-- Zola

zola1 :: CardDef
zola1 = (villain "04109" "Zola" [Android, Hydra]) {cdKeywords = singleton (Retaliate 1)}

zola2 :: CardDef
zola2 = (villain "04110" "Zola" [Android, Hydra]) {cdKeywords = singleton (Retaliate 1)}

zola3 :: CardDef
zola3 = (villain "04111" "Zola" [Android, Hydra]) {cdKeywords = singleton (Retaliate 1)}
