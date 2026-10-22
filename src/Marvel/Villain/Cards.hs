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
  , normanOsborn1
  , greenGoblin1
  , normanOsborn2
  , greenGoblin2
  , normanOsborn3
  , greenGoblin3
  , greenGoblinMutagen1
  , greenGoblinMutagen2
  , greenGoblinMutagen3

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , crossbones1
  , crossbones2
  , crossbones3
  , absorbingMan1
  , absorbingMan2
  , absorbingMan3
  -- Zola
  , zola1
  , zola2
  , zola3

  -- Red Skull
  , redSkull1
  , redSkull2
  , redSkull3

  -- Taskmaster
  , taskmasterVillain1
  , taskmasterVillain2
  , taskmasterVillain3

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

-- The Green Goblin

normanOsborn1 :: CardDef
normanOsborn1 = villain "02001a" "Norman Osborn" [Businessman, Genius]

greenGoblin1 :: CardDef
greenGoblin1 = villain "02001b" "Green Goblin" [Goblin]

normanOsborn2 :: CardDef
normanOsborn2 = villain "02002a" "Norman Osborn" [Businessman, Genius]

greenGoblin2 :: CardDef
greenGoblin2 = villain "02002b" "Green Goblin" [Goblin]

normanOsborn3 :: CardDef
normanOsborn3 = villain "02003a" "Norman Osborn" [Businessman, Genius]

greenGoblin3 :: CardDef
greenGoblin3 = villain "02003b" "Green Goblin" [Goblin]

greenGoblinMutagen1 :: CardDef
greenGoblinMutagen1 = villain "02014" "Green Goblin" [Goblin]

greenGoblinMutagen2 :: CardDef
greenGoblinMutagen2 = villain "02015" "Green Goblin" [Goblin]

greenGoblinMutagen3 :: CardDef
greenGoblinMutagen3 = villain "02016" "Green Goblin" [Goblin]
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

-- The Rise of Red Skull
-- Zola

zola1 :: CardDef
zola1 = (villain "04109" "Zola" [Android, Hydra]) {cdKeywords = singleton (Retaliate 1)}

zola2 :: CardDef
zola2 = (villain "04110" "Zola" [Android, Hydra]) {cdKeywords = singleton (Retaliate 1)}

zola3 :: CardDef
zola3 = (villain "04111" "Zola" [Android, Hydra]) {cdKeywords = singleton (Retaliate 1)}

-- Red Skull

redSkull1 :: CardDef
redSkull1 = villain "04125" "Red Skull" [Hydra]

redSkull2 :: CardDef
redSkull2 = villain "04126" "Red Skull" [Hydra]

redSkull3 :: CardDef
redSkull3 = villain "04127" "Red Skull" [Hydra]

-- The Rise of Red Skull

-- Taskmaster

taskmasterVillain1 :: CardDef
taskmasterVillain1 = villain "04093" "Taskmaster" [Hydra, Mercenary]

taskmasterVillain2 :: CardDef
taskmasterVillain2 = villain "04094" "Taskmaster" [Hydra, Mercenary]

taskmasterVillain3 :: CardDef
taskmasterVillain3 = villain "04095" "Taskmaster" [Hydra, Mercenary]
