module Marvel.Minion.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Data.HashSet (singleton)
import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Keyword
import Marvel.Name
import Marvel.Trait hiding (MastersOfEvil, Temporal)
import Marvel.Trait qualified as Trait

allMinions :: HashMap CardCode CardDef
allMinions =
  fromList $
    map
      (toFst toCardCode)
      [ hydraMercenary
      , sandman
      , shocker
      , hydraBomber
      , armoredGuard
      , weaponsRunner
      , radioactiveMan
      , whirlwind
      , tigerShark
      , melter
      , killmonger
      , titania
      , vulture
      , whiplash
      , yonRogg
      , advancedUltronDrone
      , madameHydra
      , hydraSoldier
      , modok
      , baronZemo
      , hydraSoldierCaptainAmerica
      , thomasEdison
      , edisonsGiantRobot
      , taskmaster
      , hydraMercenaryBlackWidow
      , loki
      , frostGiant
      , baronMordo
      , abomination
      -- The Green Goblin
      , scorpion
      , electro
      , tombstone

      -- The Wrecking Crew

      -- The Rise of Red Skull
      -- Taskmaster
      , hydraHunter

      -- The Once and Future Kang
      , ancientWarrior
      , chitauriSoldier
      , tyrannosaurusRex
      , apocryphus
      , deathunt9000
      , sirRaston
      , terminatrix
      , wildrun
      , kangMasterOfTime
      , timeDisplacedSoldier

      ]

allSpecialMinions :: HashMap CardCode CardDef
allSpecialMinions =
  fromList $
    map
      (toFst toCardCode)
      [ drone
      ]

minion ::
  CardCode ->
  Name ->
  [Trait] ->
  [BoostIcon] ->
  EncounterSet ->
  Natural ->
  CardDef
minion code name traits boostIcons encounterSet quantity =
  CardDef
    { cdCardCode = code
    , cdName = name
    , cdCost = Nothing
    , cdTraits = fromList traits
    , cdKeywords = mempty
    , cdCardType = MinionType
    , cdAbilityType = Nothing
    , cdAbilitySubType = Nothing
    , cdUnique = False
    , cdAspect = Nothing
    , cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just quantity
    , cdCriteria = NoCriteria
    , cdResources = []
    , cdResponseWindow = Nothing
    , cdBoostIcons = boostIcons
    , cdHazards = 0
    , cdAcceleration = 0
    , cdLimit = Nothing
    , cdAlternateCardCodes = []
    , cdPlayableFromDiscard = False
    , cdAbilityCost = NoCost
    }

unique :: CardDef -> CardDef
unique def = def {cdUnique = True}

hydraMercenary :: CardDef
hydraMercenary =
  (minion "01101" "Hydra Mercenary" [Hydra] [Boost] Rhino 2)
    { cdKeywords = singleton Guard
    }

sandman :: CardDef
sandman =
  unique $
    (minion "01102" "Sandman" [Criminal, Elite] [Boost, Boost] Rhino 1)
      { cdKeywords = singleton Toughness
      }

shocker :: CardDef
shocker = unique $ minion "01103" "Shocker" [Criminal] [Boost, Boost] Rhino 1

hydraBomber :: CardDef
hydraBomber = minion "01110" "Hydra Bomber" [Hydra] [Boost] BombScare 2

armoredGuard :: CardDef
armoredGuard =
  (minion "01120" "Armored Guard" [Mercenary] [Boost] Klaw 3)
    { cdKeywords = fromList [Guard, Toughness]
    }

weaponsRunner :: CardDef
weaponsRunner =
  (minion "01121" "Weapons Runner" [Mercenary] [Star] Klaw 2)
    { cdKeywords = singleton Surge
    }

radioactiveMan :: CardDef
radioactiveMan =
  unique $
    minion
      "01129"
      "Radioactive Man"
      [Elite, Trait.MastersOfEvil]
      [Star]
      MastersOfEvil
      1

whirlwind :: CardDef
whirlwind =
  unique $
    minion "01130" "Whirlwind" [Trait.MastersOfEvil] [Star] MastersOfEvil 1

tigerShark :: CardDef
tigerShark =
  unique $
    minion "01131" "Tiger Shark" [Trait.MastersOfEvil] [Star] MastersOfEvil 1

melter :: CardDef
melter =
  unique $ minion "01132" "Melter" [Trait.MastersOfEvil] [Star] MastersOfEvil 1

killmonger :: CardDef
killmonger =
  unique $
    minion
      "01157"
      "Killmonger"
      [Assassin, Elite, Mercenary]
      [Boost, Boost]
      BlackPantherNemesis
      1

titania :: CardDef
titania =
  unique $
    minion "01162" "Titania" [Brute, Elite] [Boost, Boost] SheHulkNemesis 1

vulture :: CardDef
vulture =
  unique $
    (minion "01167" "Vulture" [Criminal] [Boost, Boost] SpiderManNemesis 1)
      { cdKeywords = singleton Quickstrike
      }

whiplash :: CardDef
whiplash =
  unique $
    ( minion "01172" "Whiplash" [Criminal] [Boost, Boost] IronManNemesis 1
    )
      { cdKeywords = singleton (Retaliate 1)
      }

yonRogg :: CardDef
yonRogg =
  unique $
    minion
      "01177"
      "Yon-Rogg"
      [Elite, Kree]
      [Boost, Boost]
      CaptainMarvelNemesis
      1

drone :: CardDef
drone = minion "xdrone" "Drone" [Drone] [] Ultron 0

advancedUltronDrone :: CardDef
advancedUltronDrone =
  (minion "01143" "Advanced Ultron Drone" [Drone] [Boost, Boost] Ultron 3)
    { cdKeywords = singleton Guard
    }

madameHydra :: CardDef
madameHydra =
  unique $ minion "01181" "Madame Hydra" [Elite, Hydra] [Boost, Boost] LegionsOfHydra 1

hydraSoldier :: CardDef
hydraSoldier =
  (minion "01182" "Hydra Soldier" [Hydra] [Boost] LegionsOfHydra 3)
    { cdKeywords = singleton Guard
    }

modok :: CardDef
modok =
  unique $
    (minion "01184" "M.O.D.O.K." [Cyborg, Elite] [Boost, Boost] TheDoomsdayChair 1)
      { cdKeywords = singleton (Retaliate 2)
      }

baronZemo :: CardDef
baronZemo =
  unique $
    (minion "03028" "Baron Zemo" [Hydra, Elite] [Boost, Boost] CaptainAmericaNemesis 1)
      { cdKeywords = singleton Quickstrike
      }

-- | The core set's Hydra Soldier, two of them in Captain America's nemesis set.
hydraSoldierCaptainAmerica :: CardDef
hydraSoldierCaptainAmerica =
  (reprint "03029" hydraSoldier)
    { cdEncounterSet = Just CaptainAmericaNemesis
    , cdEncounterSetQuantity = Just 2
    }

-- | The nemesis minions named so on their cards, in nemesis sets with more than one
-- minion.
designatedNemesisMinions :: [CardDef]
designatedNemesisMinions = [baronZemo, thomasEdison, loki, taskmaster]

thomasEdison :: CardDef
thomasEdison =
  unique $ minion "05027" "Thomas Edison" [Bird, Genius] [Boost, Boost] MsMarvelNemesis 1

edisonsGiantRobot :: CardDef
edisonsGiantRobot =
  unique $ minion "05028" "Edison's Giant Robot" [Robot] [Boost] MsMarvelNemesis 1
abomination :: CardDef
abomination =
  unique $ minion "10026" "Abomination" [Elite, Gamma] [Boost, Boost, Boost] HulkNemesis 1

taskmaster :: CardDef
taskmaster =
  (minion "08026" "Taskmaster" [Hydra, Elite] [Star] BlackWidowNemesis 1)
    { cdUnique = True
    }

-- | Hydra Mercenary in Black Widow's nemesis set.
hydraMercenaryBlackWidow :: CardDef
hydraMercenaryBlackWidow =
  (reprint "08028" hydraMercenary)
    { cdEncounterSet = Just BlackWidowNemesis
    , cdEncounterSetQuantity = Just 2
    }

loki :: CardDef
loki = unique $ minion "06028" "Loki" [Trait.Asgard] [Boost, Boost, Boost] ThorNemesis 1

frostGiant :: CardDef
frostGiant =
  (minion "06029" "Frost Giant" [Giant] [Star, Boost] ThorNemesis 2)
    { cdKeywords = singleton Toughness
    }

baronMordo :: CardDef
baronMordo =
  unique $
    minion "09028" "Baron Mordo" [Elite, Mystic] [Boost, Boost] DoctorStrangeNemesis 1

-- The Green Goblin

scorpion :: CardDef
scorpion =
  unique $
    (minion "02038" "Scorpion" [] [Boost, Boost] AMessOfThings 1)
      { cdKeywords = singleton Quickstrike
      }

electro :: CardDef
electro = unique $ minion "02042" "Electro" [Criminal] [Star, Boost, Boost] PowerDrain 1

tombstone :: CardDef
tombstone = unique $ minion "02047" "Tombstone" [Criminal] [Boost, Boost] RunningInterference 1

-- The Once and Future Kang

ancientWarrior :: CardDef
ancientWarrior =
  (minion "11030" "Ancient Warrior" [Soldier, Trait.Temporal] [Star] Temporal 3)
    { cdKeywords = singleton Quickstrike
    }

chitauriSoldier :: CardDef
chitauriSoldier = minion "11031" "Chitauri Soldier" [Soldier, Trait.Temporal] [Boost] Temporal 2

tyrannosaurusRex :: CardDef
tyrannosaurusRex =
  (minion "11032" "Tyrannosaurus Rex" [Creature, Trait.Temporal] [Boost, Boost, Boost] Temporal 1)
    { cdKeywords = singleton Toughness
    }

apocryphus :: CardDef
apocryphus = unique $ minion "11040" "Apocryphus" [Elite, Trait.Temporal] [Star] Anachronauts 1

deathunt9000 :: CardDef
deathunt9000 =
  unique $
    (minion "11041" "Deathunt 9000" [Elite, Trait.Temporal] [Star] Anachronauts 1)
      { cdKeywords = fromList [Toughness, Villainous]
      }

sirRaston :: CardDef
sirRaston =
  unique $
    (minion "11042" "Sir Raston" [Elite, Trait.Temporal] [Star] Anachronauts 1)
      { cdKeywords = fromList [Guard, Retaliate 1]
      }

terminatrix :: CardDef
terminatrix =
  unique $
    (minion "11043" "Terminatrix" [Elite, Trait.Temporal] [Star] Anachronauts 1)
      { cdKeywords = singleton Quickstrike
      }

wildrun :: CardDef
wildrun = unique $ minion "11044" "Wildrun" [Elite, Trait.Temporal] [Star] Anachronauts 1

kangMasterOfTime :: CardDef
kangMasterOfTime =
  unique $
    (minion "11047" ("Kang" <:> "Master of Time") [Elite, Trait.Temporal] [Boost, Boost] MasterOfTime 1)
      { cdKeywords = fromList [Toughness, Villainous]
      }

timeDisplacedSoldier :: CardDef
timeDisplacedSoldier =
  (minion "11048" "Time-Displaced Soldier" [Elite, Trait.Temporal] [Star] MasterOfTime 2)
    { cdKeywords = fromList [Incite 1, Surge]
    }

-- The Rise of Red Skull

-- Taskmaster

hydraHunter :: CardDef
hydraHunter = minion "04101" "Hydra Hunter" [Hydra] [Star] Taskmaster 2
