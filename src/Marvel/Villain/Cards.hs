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
  , wreckerA
  , wreckerB
  , thunderballA
  , thunderballB
  , piledriverA
  , piledriverB
  , bulldozerA
  , bulldozerB

  -- The Rise of Red Skull

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
