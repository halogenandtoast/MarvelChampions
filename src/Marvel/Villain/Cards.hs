module Marvel.Villain.Cards where





import Marvel.Resource.Types qualified as NEResource ()

import Marvel.Boost qualified as NEBoost ()

import Marvel.GameValue.Types qualified as NEValue ()

import Marvel.EncounterSet qualified as NESet

import Marvel.Keyword qualified as NEKeyword

import Marvel.Trait qualified as NETrait

import Marvel.Aspect qualified as NEAspect ()

import Marvel.Trait qualified as MojoTrait
import Marvel.EncounterSet qualified as MojoSet
import Marvel.Keyword qualified as MojoKeyword ()
import Marvel.GameValue.Types qualified as MojoValue ()
import Marvel.Boost qualified as MojoBoost ()

import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait
import Marvel.Keyword qualified as SMKeyword
import Marvel.GameValue.Types qualified as SMValue ()
import Marvel.Boost qualified as SMBoost ()

import Data.HashSet (singleton)
import Marvel.EncounterSet (EncounterSet (Bulldozer, Piledriver, Thunderball, Wrecker))
import Marvel.Trait qualified as MGTrait
import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.GameValue.Types qualified as MGValue ()
import Marvel.Boost qualified as MGBoost ()
import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Trait
import Marvel.Keyword

allVillainCards :: [CardDef]
allVillainCards =
  [ unus1, unus2, unus3, warA, warB, famineA, famineB, pestilenceA, pestilenceB, deathA, deathB, apocalypse1, apocalypse2, apocalypse3, apocalypse4, darkBeast1, darkBeast2, darkBeast3, enSabahNurBiomorph1, enSabahNurCyberpath1, enSabahNurGiant1, enSabahNurBiomorph2, enSabahNurCyberpath2, enSabahNurGiant2, enSabahNurBiomorph3, enSabahNurCyberpath3, enSabahNurGiant3
  , ne40070a, ne40070b, ne40071a, ne40071b, ne40072a, ne40072b, ne40073a, ne40073b, ne40074a, ne40074b, ne40075a, ne40075b, ne40076a, ne40076b, ne40118, ne40119, ne40120, ne40136, ne40137, ne40138, ne40163, ne40164, ne40165
  , mojo39001a, mojo39001b, mojo39012a, mojo39012b, mojo39013a, mojo39013b, mojo39014a, mojo39014b, mojo39022, mojo39023, mojo39024
  , mgSabretooth60, mgSabretooth61, mgSabretooth62, mgSentinel84, mgSentinel85, mgSentinel86, mgMasterMold09, mgMasterMold10, mgMasterMold11, mgAvalancheA, mgAvalancheB, mgBlobA, mgBlobB, mgPyroA, mgPyroB, mgToadA, mgToadB, mgMagneto38, mgMagneto39, mgMagneto40, smSandman1, smSandman2, smSandman3, smVenom1, smVenom2, smVenom3, smMysterio1, smMysterio2, smMysterio3, smDoctorOctopus1, smElectro1, smHobgoblin1, smKravenTheHunter1, smScorpion1, smVulture1, smVenomGoblin1, smVenomGoblin2, smVenomGoblin3
  , theHood1
  , theHood2
  , theHood3
  , rhino1
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

  -- The Galaxy's Most Wanted: Brotherhood of Badoon
  , drang1
  , drang2
  , drang3

  -- The Galaxy's Most Wanted: Escape the Museum
  , collectorA1
  , collectorA2
  , collectorB1
  , collectorB2

  -- Ronan the Accuser
  , ronan1
  , ronan2
  , ronan3
  -- The Galaxy's Most Wanted: Nebula
  , nebula1
  , nebula2
  , nebula3
  -- The Galaxy's Most Wanted: Infiltrate the Museum
  , collector1
  , collector2
  , collector3

  -- The Mad Titan's Shadow: Ebony Maw
  , ebonyMaw1
  , ebonyMaw2
  , ebonyMaw3

  -- The Mad Titan's Shadow: Tower Defense
  , proximaMidnight1
  , proximaMidnight2
  , proximaMidnight3
  , corvusGlaive1
  , corvusGlaive2
  , corvusGlaive3

  -- The Mad Titan's Shadow: Thanos
  , thanos1
  , thanos2
  , thanos3

  -- The Mad Titan's Shadow: Hela (one card, Mystic and Wounded sides)
  , helaA1
  , helaA2
  , helaB1
  , helaB2

  -- The Mad Titan's Shadow: Loki
  , loki1
  , loki2
  , loki3
  , loki4
  , loki5
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
    , cdDeckLimit = Nothing
    , cdTeamUp = Nothing
    , cdAlternateCardCodes = []
    , cdPlayableFromDiscard = False
    , cdDoubleWildResources = False
    , cdDoesNotCountTowardHandSize = False
    , cdCanPayForOthersWithTrait = Nothing
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

-- The Galaxy's Most Wanted: Brotherhood of Badoon

drang1 :: CardDef
drang1 = villain "16058" "Drang" [Badoon]

drang2 :: CardDef
drang2 = villain "16059" "Drang" [Badoon]

drang3 :: CardDef
drang3 = villain "16060" "Drang" [Badoon]

-- The Galaxy's Most Wanted: Escape the Museum

-- | Collector (A1), Collector (B1) for expert play, each with his Wounded side (A2, B2).
collectorA1 :: CardDef
collectorA1 = villain "16080a" "Collector" [Elder]

collectorA2 :: CardDef
collectorA2 = villain "16080b" "Collector" [Elder, Wounded]

collectorB1 :: CardDef
collectorB1 = villain "16081a" "Collector" [Elder]

collectorB2 :: CardDef
collectorB2 = villain "16081b" "Collector" [Elder, Wounded]

-- Ronan the Accuser

ronan1 :: CardDef
ronan1 = (villain "16103" "Ronan the Accuser" [AccuserCorps, Kree]) {cdKeywords = singleton Toughness}

ronan2 :: CardDef
ronan2 = (villain "16104" "Ronan the Accuser" [AccuserCorps, Kree]) {cdKeywords = singleton Toughness}

ronan3 :: CardDef
ronan3 = (villain "16105" "Ronan the Accuser" [AccuserCorps, Kree]) {cdKeywords = fromList [Retaliate 1, Toughness]}

-- The Galaxy's Most Wanted: Nebula

nebula1 :: CardDef
nebula1 = villain "16088" "Nebula" [Criminal]

nebula2 :: CardDef
nebula2 = villain "16089" "Nebula" [Criminal]

nebula3 :: CardDef
nebula3 = villain "16090" "Nebula" [Criminal]

-- The Galaxy's Most Wanted: Infiltrate the Museum

collector1 :: CardDef
collector1 = villain "16070" "Collector" [Elder]

collector2 :: CardDef
collector2 = villain "16071" "Collector" [Elder]

collector3 :: CardDef
collector3 = villain "16072" "Collector" [Elder]

-- The Mad Titan's Shadow: Ebony Maw

ebonyMaw1 :: CardDef
ebonyMaw1 = villain "21071" "Ebony Maw" [BlackOrder, Mystic]

ebonyMaw2 :: CardDef
ebonyMaw2 = villain "21072" "Ebony Maw" [BlackOrder, Mystic]

ebonyMaw3 :: CardDef
ebonyMaw3 = villain "21073" "Ebony Maw" [BlackOrder, Mystic]

-- The Mad Titan's Shadow: Tower Defense

proximaMidnight1 :: CardDef
proximaMidnight1 = villain "21092" "Proxima Midnight" [BlackOrder]

proximaMidnight2 :: CardDef
proximaMidnight2 = villain "21093" "Proxima Midnight" [BlackOrder]

proximaMidnight3 :: CardDef
proximaMidnight3 = villain "21094" "Proxima Midnight" [BlackOrder]

corvusGlaive1 :: CardDef
corvusGlaive1 = villain "21095" "Corvus Glaive" [BlackOrder]

corvusGlaive2 :: CardDef
corvusGlaive2 = villain "21096" "Corvus Glaive" [BlackOrder]

corvusGlaive3 :: CardDef
corvusGlaive3 = villain "21097" "Corvus Glaive" [BlackOrder]

-- The Mad Titan's Shadow: Thanos

thanos1 :: CardDef
thanos1 = (villain "21111" "Thanos" [BlackOrder, Titan]) {cdKeywords = singleton Stalwart}

thanos2 :: CardDef
thanos2 = (villain "21112" "Thanos" [BlackOrder, Titan]) {cdKeywords = fromList [Stalwart, Toughness]}

thanos3 :: CardDef
thanos3 = (villain "21113" "Thanos" [BlackOrder, Titan]) {cdKeywords = fromList [Stalwart, Toughness]}

-- The Mad Titan's Shadow: Loki
-- five versions of one stage I, each with victory 1 (MTS rulebook p.24)

loki1 :: CardDef
loki1 = (villain "21160" "Loki" [Asgard, Mystic]) {cdKeywords = singleton (Victory 1)}

loki2 :: CardDef
loki2 = (villain "21161" "Loki" [Asgard, Mystic]) {cdKeywords = fromList [Retaliate 1, Victory 1]}

loki3 :: CardDef
loki3 = (villain "21162" "Loki" [Asgard, Mystic]) {cdKeywords = fromList [Stalwart, Victory 1]}

loki4 :: CardDef
loki4 = (villain "21163" "Loki" [Asgard, Mystic]) {cdKeywords = fromList [Stalwart, Victory 1]}

-- | "[star] Loki's attacks gain piercing": the keyword on his card.
loki5 :: CardDef
loki5 = (villain "21164" "Loki" [Asgard, Mystic]) {cdKeywords = fromList [Piercing, Victory 1]}

-- The Mad Titan's Shadow: Hela

helaA1 :: CardDef
helaA1 = villain "21136a" "Hela" [Asgard, Mystic]

helaA2 :: CardDef
helaA2 = villain "21136b" "Hela" [Asgard, Wounded]

helaB1 :: CardDef
helaB1 = villain "21137a" "Hela" [Asgard, Mystic]

helaB2 :: CardDef
helaB2 = villain "21137b" "Hela" [Asgard, Wounded]

-- The Hood

theHood1 :: CardDef
theHood1 = villain "24001" "The Hood" [Criminal]

theHood2 :: CardDef
theHood2 = villain "24002" "The Hood" [Criminal]

theHood3 :: CardDef
theHood3 = villain "24003" "The Hood" [Criminal]

-- Sinister Motives

smVillainDef :: CardCode -> Name -> SMSet.EncounterSet -> Natural -> CardDef
smVillainDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just set
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

smSandman1 :: CardDef
smSandman1 = (smVillainDef "27061" "Sandman" SMSet.Sandman 1)
  { cdTraits = fromList [SMTrait.Criminal]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSandman2 :: CardDef
smSandman2 = (smVillainDef "27062" "Sandman" SMSet.Sandman 1)
  { cdTraits = fromList [SMTrait.Criminal]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSandman3 :: CardDef
smSandman3 = (smVillainDef "27063" "Sandman" SMSet.Sandman 1)
  { cdTraits = fromList [SMTrait.Criminal]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smVenom1 :: CardDef
smVenom1 = (smVillainDef "27073" "Venom" SMSet.Venom 1)
  { cdTraits = fromList [SMTrait.Symbiote]
  , cdKeywords = fromList [SMKeyword.Toughness]
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smVenom2 :: CardDef
smVenom2 = (smVillainDef "27074" "Venom" SMSet.Venom 1)
  { cdTraits = fromList [SMTrait.Symbiote]
  , cdKeywords = fromList [SMKeyword.Toughness, SMKeyword.Steady]
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smVenom3 :: CardDef
smVenom3 = (smVillainDef "27075" "Venom" SMSet.Venom 1)
  { cdTraits = fromList [SMTrait.Symbiote]
  , cdKeywords = fromList [SMKeyword.Retaliate 1, SMKeyword.Steady, SMKeyword.Toughness]
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smMysterio1 :: CardDef
smMysterio1 = (smVillainDef "27084" "Mysterio" SMSet.Mysterio 1)
  { cdTraits = fromList [SMTrait.Criminal]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smMysterio2 :: CardDef
smMysterio2 = (smVillainDef "27085" "Mysterio" SMSet.Mysterio 1)
  { cdTraits = fromList [SMTrait.Criminal]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smMysterio3 :: CardDef
smMysterio3 = (smVillainDef "27086" "Mysterio" SMSet.Mysterio 1)
  { cdTraits = fromList [SMTrait.Criminal]
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smDoctorOctopus1 :: CardDef
smDoctorOctopus1 = (smVillainDef "27094" "Doctor Octopus" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smElectro1 :: CardDef
smElectro1 = (smVillainDef "27095" "Electro" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smHobgoblin1 :: CardDef
smHobgoblin1 = (smVillainDef "27096" "Hobgoblin" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smKravenTheHunter1 :: CardDef
smKravenTheHunter1 = (smVillainDef "27097" "Kraven the Hunter" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smScorpion1 :: CardDef
smScorpion1 = (smVillainDef "27098" "Scorpion" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smVulture1 :: CardDef
smVulture1 = (smVillainDef "27099" "Vulture" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smVenomGoblin1 :: CardDef
smVenomGoblin1 = (smVillainDef "27113" "Venom Goblin" SMSet.VenomGoblin 1)
  { cdTraits = fromList [SMTrait.Goblin, SMTrait.Symbiote]
  , cdKeywords = fromList [SMKeyword.Steady]
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smVenomGoblin2 :: CardDef
smVenomGoblin2 = (smVillainDef "27114" "Venom Goblin" SMSet.VenomGoblin 1)
  { cdTraits = fromList [SMTrait.Goblin, SMTrait.Symbiote]
  , cdKeywords = fromList [SMKeyword.Steady, SMKeyword.Toughness]
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smVenomGoblin3 :: CardDef
smVenomGoblin3 = (smVillainDef "27115" "Venom Goblin" SMSet.VenomGoblin 1)
  { cdTraits = fromList [SMTrait.Goblin, SMTrait.Symbiote]
  , cdKeywords = fromList [SMKeyword.Retaliate 1, SMKeyword.Stalwart, SMKeyword.Toughness]
  , cdUnique = True
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

-- Mutant Genesis scenario definitions
mgSabretooth60 :: CardDef
mgSabretooth60 = (smVillainDef "32060" ("Sabretooth") MGSet.Sabretooth 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgSabretooth61 :: CardDef
mgSabretooth61 = (smVillainDef "32061" ("Sabretooth") MGSet.Sabretooth 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness]
  }

mgSabretooth62 :: CardDef
mgSabretooth62 = (smVillainDef "32062" ("Sabretooth") MGSet.Sabretooth 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Retaliate 1]
  }

mgSentinel84 :: CardDef
mgSentinel84 = (smVillainDef "32084" ("Sentinel") MGSet.ProjectWideawake 0)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness]
  }

mgSentinel85 :: CardDef
mgSentinel85 = (smVillainDef "32085" ("Sentinel") MGSet.ProjectWideawake 0)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Steady]
  }

mgSentinel86 :: CardDef
mgSentinel86 = (smVillainDef "32086" ("Sentinel") MGSet.ProjectWideawake 0)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Stalwart]
  }

mgMasterMold09 :: CardDef
mgMasterMold09 = (smVillainDef "32109" ("Master Mold") MGSet.MasterMold 0)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Stalwart]
  }

mgMasterMold10 :: CardDef
mgMasterMold10 = (smVillainDef "32110" ("Master Mold") MGSet.MasterMold 0)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Stalwart]
  }

mgMasterMold11 :: CardDef
mgMasterMold11 = (smVillainDef "32111" ("Master Mold") MGSet.MasterMold 0)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Stalwart]
  }

mgAvalancheA :: CardDef
mgAvalancheA = (smVillainDef "32121a" ("Avalanche") MGSet.MansionAttack 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Victory 2]
  }

mgAvalancheB :: CardDef
mgAvalancheB = (smVillainDef "32121b" ("Avalanche") MGSet.MansionAttack 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Victory 2]
  }

mgBlobA :: CardDef
mgBlobA = (smVillainDef "32122a" ("Blob") MGSet.MansionAttack 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Victory 2]
  }

mgBlobB :: CardDef
mgBlobB = (smVillainDef "32122b" ("Blob") MGSet.MansionAttack 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Victory 2]
  }

mgPyroA :: CardDef
mgPyroA = (smVillainDef "32123a" ("Pyro") MGSet.MansionAttack 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Victory 2]
  }

mgPyroB :: CardDef
mgPyroB = (smVillainDef "32123b" ("Pyro") MGSet.MansionAttack 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Victory 2]
  }

mgToadA :: CardDef
mgToadA = (smVillainDef "32124a" ("Toad") MGSet.MansionAttack 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Victory 2]
  }

mgToadB :: CardDef
mgToadB = (smVillainDef "32124b" ("Toad") MGSet.MansionAttack 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Victory 2]
  }

mgMagneto38 :: CardDef
mgMagneto38 = (smVillainDef "32138" ("Magneto") MGSet.Magneto 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Steady]
  }

mgMagneto39 :: CardDef
mgMagneto39 = (smVillainDef "32139" ("Magneto") MGSet.Magneto 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Steady]
  }

mgMagneto40 :: CardDef
mgMagneto40 = (smVillainDef "32140" ("Magneto") MGSet.Magneto 0)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Steady]
  }


-- Mojo Mania

mojoCardDef :: CardCode -> Name -> MojoSet.EncounterSet -> Natural -> CardDef
mojoCardDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just set
  , cdEncounterSetQuantity = Just quantity
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

mojo39001a :: CardDef
mojo39001a = (mojoCardDef "39001a" "MaGog" MojoSet.Magog 0)
  { cdTraits = fromList [MojoTrait.Brute]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39001b :: CardDef
mojo39001b = (mojoCardDef "39001b" "MaGog" MojoSet.Magog 0)
  { cdTraits = fromList [MojoTrait.Brute]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39012a :: CardDef
mojo39012a = (mojoCardDef "39012a" "Spiral" MojoSet.Spiral 0)
  { cdTraits = fromList [MojoTrait.Escaped, MojoTrait.Mystic]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39012b :: CardDef
mojo39012b = (mojoCardDef "39012b" "Spiral" MojoSet.Spiral 0)
  { cdTraits = fromList [MojoTrait.Cornered, MojoTrait.Mystic]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39013a :: CardDef
mojo39013a = (mojoCardDef "39013a" "Spiral" MojoSet.Spiral 0)
  { cdTraits = fromList [MojoTrait.Escaped, MojoTrait.Mystic]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39013b :: CardDef
mojo39013b = (mojoCardDef "39013b" "Spiral" MojoSet.Spiral 0)
  { cdTraits = fromList [MojoTrait.Cornered, MojoTrait.Mystic]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39014a :: CardDef
mojo39014a = (mojoCardDef "39014a" "Spiral" MojoSet.Spiral 0)
  { cdTraits = fromList [MojoTrait.Escaped, MojoTrait.Mystic]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39014b :: CardDef
mojo39014b = (mojoCardDef "39014b" "Spiral" MojoSet.Spiral 0)
  { cdTraits = fromList [MojoTrait.Cornered, MojoTrait.Mystic]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39022 :: CardDef
mojo39022 = (mojoCardDef "39022" "Mojo" MojoSet.Mojo 0)
  { cdTraits = fromList [MojoTrait.Spineless]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39023 :: CardDef
mojo39023 = (mojoCardDef "39023" "Mojo" MojoSet.Mojo 0)
  { cdTraits = fromList [MojoTrait.Spineless]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

mojo39024 :: CardDef
mojo39024 = (mojoCardDef "39024" "Mojo" MojoSet.Mojo 0)
  { cdTraits = fromList [MojoTrait.Spineless]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = []
  }

-- NeXt Evolution campaign and scenarios

ne40070a :: CardDef
ne40070a = CardDef
  { cdCardCode = "40070a"
  , cdName = "Arclight"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Marauder]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40070b :: CardDef
ne40070b = CardDef
  { cdCardCode = "40070b"
  , cdName = "Arclight"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Marauder]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40071a :: CardDef
ne40071a = CardDef
  { cdCardCode = "40071a"
  , cdName = "Blockbuster"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Marauder]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40071b :: CardDef
ne40071b = CardDef
  { cdCardCode = "40071b"
  , cdName = "Blockbuster"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Marauder]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40072a :: CardDef
ne40072a = CardDef
  { cdCardCode = "40072a"
  , cdName = "Chimera"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder, NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40072b :: CardDef
ne40072b = CardDef
  { cdCardCode = "40072b"
  , cdName = "Chimera"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder, NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40073a :: CardDef
ne40073a = CardDef
  { cdCardCode = "40073a"
  , cdName = "Greycrow"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder, NETrait.Mercenary]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40073b :: CardDef
ne40073b = CardDef
  { cdCardCode = "40073b"
  , cdName = "Greycrow"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder, NETrait.Mercenary]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40074a :: CardDef
ne40074a = CardDef
  { cdCardCode = "40074a"
  , cdName = "Harpoon"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Marauder]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40074b :: CardDef
ne40074b = CardDef
  { cdCardCode = "40074b"
  , cdName = "Harpoon"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Marauder]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40075a :: CardDef
ne40075a = CardDef
  { cdCardCode = "40075a"
  , cdName = "Riptide"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Assassin, NETrait.Marauder]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40075b :: CardDef
ne40075b = CardDef
  { cdCardCode = "40075b"
  , cdName = "Riptide"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Assassin, NETrait.Marauder]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40076a :: CardDef
ne40076a = CardDef
  { cdCardCode = "40076a"
  , cdName = "Vertigo"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder, NETrait.Mutate]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40076b :: CardDef
ne40076b = CardDef
  { cdCardCode = "40076b"
  , cdName = "Vertigo"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder, NETrait.Mutate]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeMarauders
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40118 :: CardDef
ne40118 = CardDef
  { cdCardCode = "40118"
  , cdName = "Juggernaut"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeJuggernaut
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40119 :: CardDef
ne40119 = CardDef
  { cdCardCode = "40119"
  , cdName = "Juggernaut"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeJuggernaut
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40120 :: CardDef
ne40120 = CardDef
  { cdCardCode = "40120"
  , cdName = "Juggernaut"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeJuggernaut
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40136 :: CardDef
ne40136 = CardDef
  { cdCardCode = "40136"
  , cdName = "Mister Sinister"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Genius]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.MisterSinister
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40137 :: CardDef
ne40137 = CardDef
  { cdCardCode = "40137"
  , cdName = "Mister Sinister"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Genius]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.MisterSinister
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40138 :: CardDef
ne40138 = CardDef
  { cdCardCode = "40138"
  , cdName = "Mister Sinister"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Genius]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.MisterSinister
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40163 :: CardDef
ne40163 = CardDef
  { cdCardCode = "40163"
  , cdName = "Stryfe"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront, NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeStryfe
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40164 :: CardDef
ne40164 = CardDef
  { cdCardCode = "40164"
  , cdName = "Stryfe"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront, NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeStryfe
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

ne40165 :: CardDef
ne40165 = CardDef
  { cdCardCode = "40165"
  , cdName = "Stryfe"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront, NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeStryfe
  , cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Age of Apocalypse campaign and scenarios

unus1 :: CardDef
unus1 = CardDef
  { cdCardCode = "45059"
  , cdName = "Unus"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Prelate]
  , cdKeywords = fromList [NEKeyword.Toughness]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaUnus
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

unus2 :: CardDef
unus2 = CardDef
  { cdCardCode = "45060"
  , cdName = "Unus"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Prelate]
  , cdKeywords = fromList [NEKeyword.Toughness]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaUnus
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

unus3 :: CardDef
unus3 = CardDef
  { cdCardCode = "45061"
  , cdName = "Unus"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Prelate]
  , cdKeywords = fromList [NEKeyword.Toughness]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaUnus
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

warA :: CardDef
warA = CardDef
  { cdCardCode = "45081a"
  , cdName = "War"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Horsemen]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

warB :: CardDef
warB = CardDef
  { cdCardCode = "45081b"
  , cdName = "War"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Horsemen]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

famineA :: CardDef
famineA = CardDef
  { cdCardCode = "45082a"
  , cdName = "Famine"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Horsemen]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

famineB :: CardDef
famineB = CardDef
  { cdCardCode = "45082b"
  , cdName = "Famine"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Horsemen]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

pestilenceA :: CardDef
pestilenceA = CardDef
  { cdCardCode = "45083a"
  , cdName = "Pestilence"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Horsemen]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

pestilenceB :: CardDef
pestilenceB = CardDef
  { cdCardCode = "45083b"
  , cdName = "Pestilence"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Horsemen]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

deathA :: CardDef
deathA = CardDef
  { cdCardCode = "45084a"
  , cdName = "Death"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Aerial, NETrait.Horsemen]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

deathB :: CardDef
deathB = CardDef
  { cdCardCode = "45084b"
  , cdName = "Death"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Aerial, NETrait.Horsemen]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaFourHorsemen
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

apocalypse1 :: CardDef
apocalypse1 = CardDef
  { cdCardCode = "45101a"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant]
  , cdKeywords = fromList [NEKeyword.Toughness]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaApocalypse
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

apocalypse2 :: CardDef
apocalypse2 = CardDef
  { cdCardCode = "45101b"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant]
  , cdKeywords = fromList [NEKeyword.Toughness, NEKeyword.Steady]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaApocalypse
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

apocalypse3 :: CardDef
apocalypse3 = CardDef
  { cdCardCode = "45102a"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant]
  , cdKeywords = fromList [NEKeyword.Toughness, NEKeyword.Steady]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaApocalypse
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

apocalypse4 :: CardDef
apocalypse4 = CardDef
  { cdCardCode = "45102b"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant]
  , cdKeywords = fromList [NEKeyword.Toughness, NEKeyword.Stalwart]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaApocalypse
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

darkBeast1 :: CardDef
darkBeast1 = CardDef
  { cdCardCode = "45118"
  , cdName = "Dark Beast"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Genius]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaDarkBeast
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

darkBeast2 :: CardDef
darkBeast2 = CardDef
  { cdCardCode = "45119"
  , cdName = "Dark Beast"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Genius]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaDarkBeast
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

darkBeast3 :: CardDef
darkBeast3 = CardDef
  { cdCardCode = "45120"
  , cdName = "Dark Beast"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Genius]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaDarkBeast
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

enSabahNurBiomorph1 :: CardDef
enSabahNurBiomorph1 = CardDef
  { cdCardCode = "45184a"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant, NETrait.Biomorph]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

enSabahNurCyberpath1 :: CardDef
enSabahNurCyberpath1 = CardDef
  { cdCardCode = "45184b"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant, NETrait.Cyberpath]
  , cdKeywords = fromList [NEKeyword.Retaliate (1)]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

enSabahNurGiant1 :: CardDef
enSabahNurGiant1 = CardDef
  { cdCardCode = "45184c"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant, NETrait.Giant]
  , cdKeywords = fromList [NEKeyword.Stalwart]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

enSabahNurBiomorph2 :: CardDef
enSabahNurBiomorph2 = CardDef
  { cdCardCode = "45185a"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant, NETrait.Biomorph]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

enSabahNurCyberpath2 :: CardDef
enSabahNurCyberpath2 = CardDef
  { cdCardCode = "45185b"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant, NETrait.Cyberpath]
  , cdKeywords = fromList [NEKeyword.Retaliate (1)]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

enSabahNurGiant2 :: CardDef
enSabahNurGiant2 = CardDef
  { cdCardCode = "45185c"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant, NETrait.Giant]
  , cdKeywords = fromList [NEKeyword.Stalwart]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

enSabahNurBiomorph3 :: CardDef
enSabahNurBiomorph3 = CardDef
  { cdCardCode = "45186a"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant, NETrait.Biomorph]
  , cdKeywords = fromList []
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

enSabahNurCyberpath3 :: CardDef
enSabahNurCyberpath3 = CardDef
  { cdCardCode = "45186b"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant, NETrait.Cyberpath]
  , cdKeywords = fromList [NEKeyword.Retaliate (1)]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

enSabahNurGiant3 :: CardDef
enSabahNurGiant3 = CardDef
  { cdCardCode = "45186c"
  , cdName = "Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Mutant, NETrait.Giant]
  , cdKeywords = fromList [NEKeyword.Stalwart]
  , cdCardType = VillainType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaEnSabahNur
  , cdEncounterSetQuantity = Just 0
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Nothing
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }
