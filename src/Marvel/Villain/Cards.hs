module Marvel.Villain.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Trait

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
  , crossbones1
  , crossbones2
  , crossbones3
  , absorbingMan1
  , absorbingMan2
  , absorbingMan3

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

crossbones1 :: CardDef
crossbones1 = villain "04058" "Crossbones" [Hydra, Mercenary]

crossbones2 :: CardDef
crossbones2 = villain "04059" "Crossbones" [Hydra, Mercenary]

crossbones3 :: CardDef
crossbones3 = villain "04060" "Crossbones" [Hydra, Mercenary]

absorbingMan1 :: CardDef
absorbingMan1 = villain "04076" "Absorbing Man" [Brute]

absorbingMan2 :: CardDef
absorbingMan2 = villain "04077" "Absorbing Man" [Brute]

absorbingMan3 :: CardDef
absorbingMan3 = villain "04078" "Absorbing Man" [Brute]
