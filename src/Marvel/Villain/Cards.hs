module Marvel.Villain.Cards where

import Data.HashSet (singleton)
import Marvel.Keyword (Keyword (Retaliate))
import Marvel.EncounterSet (EncounterSet (Bulldozer, Piledriver, Thunderball, Wrecker))
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
  , wreckerA
  , wreckerB
  , thunderballA
  , thunderballB
  , piledriverA
  , piledriverB
  , bulldozerA
  , bulldozerB

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
  , kang1
  , kangImmortus
  , kangIronLad
  , kangRamaTut
  , kangScarletCenturion
  , kang3
  , kang1Expert
  , kangImmortusExpert
  , kangIronLadExpert
  , kangRamaTutExpert
  , kangScarletCenturionExpert
  , kang3Expert

  -- Ronan the Accuser
  , ronan1
  , ronan2
  , ronan3
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
    , cdAbilitySubTypes = []
    , cdUnique = True
    , cdAspect = Nothing
    , cdEncounterSet = Nothing
    , cdEncounterSetQuantity = Nothing
    , cdCriteria = NoCriteria
    , cdResources = []
    , cdResponseWindow = Nothing
    , cdActionCriteria = Nothing
    , cdBoostIcons = []
    , cdHazards = 0
    , cdAcceleration = 0
    , cdAmplify = 0
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

-- The Once and Future Kang

-- | Kang, the standard set's and the Expert Kang set's, each with toughness.
kangVillain :: CardCode -> Text -> CardDef
kangVillain code sub' = (villain code ("Kang" <:> sub') [Temporal]) {cdKeywords = singleton Toughness}

kang1 :: CardDef
kang1 = kangVillain "11001" "The Conqueror"

kangImmortus :: CardDef
kangImmortus = kangVillain "11002" "Immortus"

kangIronLad :: CardDef
kangIronLad = (kangVillain "11003" "Iron Lad") {cdKeywords = fromList [Retaliate 1, Toughness]}

kangRamaTut :: CardDef
kangRamaTut = kangVillain "11004" "Rama-Tut"

kangScarletCenturion :: CardDef
kangScarletCenturion = kangVillain "11005" "Scarlet Centurion"

kang3 :: CardDef
kang3 = kangVillain "11006" "The Conqueror"

kang1Expert :: CardDef
kang1Expert = kangVillain "11034" "The Conqueror"

kangImmortusExpert :: CardDef
kangImmortusExpert = kangVillain "11035" "Immortus"

kangIronLadExpert :: CardDef
kangIronLadExpert = (kangVillain "11036" "Iron Lad") {cdKeywords = fromList [Retaliate 1, Toughness]}

kangRamaTutExpert :: CardDef
kangRamaTutExpert = kangVillain "11037" "Rama-Tut"

kangScarletCenturionExpert :: CardDef
kangScarletCenturionExpert = kangVillain "11038" "Scarlet Centurion"

kang3Expert :: CardDef
kang3Expert = kangVillain "11039" "The Conqueror"

-- The Wrecking Crew

-- | The Wrecking Crew's villains, each his own encounter set: version A for standard, B
-- for expert (TWC insert p.7).
wreckingCrewVillain :: CardCode -> Name -> EncounterSet -> CardDef
wreckingCrewVillain code name set = (villain code name [WreckingCrew]) {cdEncounterSet = Just set}

wreckerA :: CardDef
wreckerA = wreckingCrewVillain "07002" "Wrecker" Wrecker

wreckerB :: CardDef
wreckerB = wreckingCrewVillain "07003" "Wrecker" Wrecker

thunderballA :: CardDef
thunderballA = wreckingCrewVillain "07017" "Thunderball" Thunderball

thunderballB :: CardDef
thunderballB = wreckingCrewVillain "07018" "Thunderball" Thunderball

piledriverA :: CardDef
piledriverA = (wreckingCrewVillain "07032" "Piledriver" Piledriver) {cdKeywords = singleton (Retaliate 1)}

piledriverB :: CardDef
piledriverB = (wreckingCrewVillain "07033" "Piledriver" Piledriver) {cdKeywords = singleton (Retaliate 1)}

bulldozerA :: CardDef
bulldozerA = wreckingCrewVillain "07046" "Bulldozer" Bulldozer

bulldozerB :: CardDef
bulldozerB = wreckingCrewVillain "07047" "Bulldozer" Bulldozer

-- Ronan the Accuser

ronan1 :: CardDef
ronan1 = (villain "16103" "Ronan the Accuser" [AccuserCorps, Kree]) {cdKeywords = singleton Toughness}

ronan2 :: CardDef
ronan2 = (villain "16104" "Ronan the Accuser" [AccuserCorps, Kree]) {cdKeywords = singleton Toughness}

ronan3 :: CardDef
ronan3 = (villain "16105" "Ronan the Accuser" [AccuserCorps, Kree]) {cdKeywords = fromList [Retaliate 1, Toughness]}
