module Marvel.Minion.Cards where

import Marvel.Keyword qualified as SpdrKeyword
import Marvel.EncounterSet qualified as SpdrSet
import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait
import Marvel.Keyword qualified as SMKeyword
import Marvel.GameValue.Types qualified as SMValue
import Marvel.Boost qualified as SMBoost

import Marvel.Ability.Types qualified as Ability
import Marvel.Cost.Types
import Marvel.Window
import Marvel.Matchers
import Marvel.Trait qualified as MGTrait
import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.GameValue.Types qualified as MGValue
import Marvel.Boost qualified as MGBoost
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
      [ mgAvalanche, mgBlob, mgPyro, mgToad, mgMystique, mgSentinelMarkIv, mgSentinelMarkIi, mgSentinelMarkIii, mgSentinelMarkV, mgSentinelMarkVi, mgSentinelMarkViii, mgMTypeSentinel, mgFabianCortez, mgAmeliaVoght, mgSenyaka, mgDelgado, mgUnuscione, mgNimrod, mgBastion, smSandClone, smShiftingApparition, smSymbioticBerserker, smSymbioticMonstrosity, smSymbioticThrall, smRhino, smCommonCriminal, smLifeSizeDecoy, smEvilDoppelganger, smDoctorOctopus, smElectro, smHobgoblin, smKravenTheHunter, smScorpion, smVulture, smEnragedSymbiote
  , madameMasque
      , hydraMercenary
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
      , hiredGun
      , privateSecuritySpecialist
      , goblinKnight
      , goblinSoldier
      , goblinThrall
      , monster
      , scorpion
      , electro
      , tombstone

      -- The Wrecking Crew
      , corruptPrisonGuard
      , corruptPrisonGuardThunderball
      , corruptPrisonGuardPiledriver
      , corruptPrisonGuardBulldozer
      , escapedConvict
      , escapedConvictThunderball
      , escapedConvictPiledriver
      , escapedConvictBulldozer

      -- The Rise of Red Skull
      , crossfire
      , theViper
      , hydraRegular
      , hydraBomberCrossbones
      , hydraFlameSoldier
      , hydraJetTrooper
      , hydraRegularHydraPatrol
      , hydraSoldierHydraPatrol

      -- Zola
      , ultimateBioServant
      , zolasMutate
      , berserkMutate

      -- Red Skull
      , theSleeper
      , hydraExoSoldier

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
      , macrobots


      -- Ant-Man
      , yellowjacket

      -- Wasp
      , beetle

      -- Quicksilver
      , avalanche

      -- Scarlet Witch
      , luminous

      -- Star-Lord
      , misterKnife

      -- Gamora
      , nebulaMinion

      -- Groot
      , furnax

      -- Rocket Raccoon
      , blackjackOHare

      -- The Galaxy's Most Wanted: Badoon Headhunter
      , badoonHeadhunter
      , headhuntersHenchman
      -- Brotherhood of Badoon
      , badoonEngineer
      -- Band of Badoon
      , badoonAssassin
      , badoonGrunt
      , badoonLieutenant
      , badoonSentry
      , badoonWarlord

      -- Kree Militants
      , kreeCommando
      , kreeLieutenant
      , kreePrivate

      -- The Galaxy's Most Wanted: Space Pirates
      , pirateCommander
      , pirateLackey

      -- The Galaxy's Most Wanted: Infiltrate the Museum
      , monarkStarstalker
      -- Menagerie Medley
      , psionicGhost
      , servantBot
      , starshark
      -- Drax
      , yotatTheDestroyer

      -- Venom
      , enragedSymbiote

      -- Spectrum
      , radioactiveManNemesis

      -- Adam Warlock
      , theMagus
      , zealotOfTruth

      -- The Mad Titan's Shadow: Black Order
      , blackDwarf
      , supergiant
      -- Armies of Titan
      , blackOrderInfantry
      , outrider

      -- The Mad Titan's Shadow: Tower Defense
      , blackOrderBesieger

      -- The Mad Titan's Shadow: Children of Thanos
      , corvusGlaiveMinion
      , proximaMidnightMinion
      , ebonyMawMinion

      -- The Mad Titan's Shadow: Hela
      , garm
      , skurge
      , nidhogg
      -- Legions of Hel
      , draugr
      -- Frost Giants
      , laufey
      , frostGiantFrostGiants
      -- Enchantress
      , enchantress

      -- The Mad Titan's Shadow: the campaign set
      , blackSwan
      , gamoraNemesis
      , livingLaser
      , enchantressValkyrie
      , ultronNemesis
  , theLizard
  , prowler
  , warbringer
  , armadillo
  , luciaVonBardas
  , zzzax
  , theGreenGobbler
  , bora
  , brix
  , daemos
  , jennix
  , karn
  , morlun
  , solus
  , verna
  , morbius
  , bombshellSinister
  , electroSinister
  , hobgoblinSinister
  , ironSpider
  , sandmanSinister
  , spot
  , juggernaut
  , whiteQueenMutantGenesis
  , hellfirePawn
  ]

allSpecialMinions :: HashMap CardCode CardDef
allSpecialMinions =
  fromList $
    map
      (toFst toCardCode)
      [ drone
      , allyMinion
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
    , cdAbilitySubTypes = []
    , cdUnique = False
    , cdAspect = Nothing
    , cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just quantity
    , cdCriteria = NoCriteria
    , cdResources = []
    , cdResponseWindow = Nothing
    , cdActionCriteria = Nothing
    , cdBoostIcons = boostIcons
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
designatedNemesisMinions = [baronZemo, thomasEdison, loki, taskmaster, theMagus]

-- | Whether the card is its nemesis set's nemesis minion: the one the set names, or its
-- only minion.
isNemesisMinion :: CardDef -> Bool
isNemesisMinion def =
  cdCardType def == MinionType
    && ( toCardCode def `elem` map toCardCode designatedNemesisMinions
          || cdEncounterSet def `notElem` map cdEncounterSet designatedNemesisMinions
       )

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

crossfire :: CardDef
crossfire =
  unique $
    (minion "04027" "Crossfire" [Mercenary] [Star] HawkeyeNemesis 1)
      { cdKeywords = fromList [Quickstrike, Piercing]
      }

theViper :: CardDef
theViper = unique $ minion "04054" "The Viper" [Elite, Hydra] [Boost, Boost, Boost] SpiderWomanNemesis 1

hydraRegular :: CardDef
hydraRegular =
  (minion "04056" "Hydra Regular" [Hydra] [Boost, Boost] SpiderWomanNemesis 2)
    { cdKeywords = fromList [Incite 1]
    }

-- | The core set's Hydra Bomber, two of them in the Crossbones set.
hydraBomberCrossbones :: CardDef
hydraBomberCrossbones =
  (reprint "04066" hydraBomber)
    { cdEncounterSet = Just Crossbones
    , cdEncounterSetQuantity = Just 2
    }

hydraFlameSoldier :: CardDef
hydraFlameSoldier = minion "04145" "Hydra Flame-Soldier" [Hydra] [Star] HydraAssault 3

hydraJetTrooper :: CardDef
hydraJetTrooper =
  (minion "04146" "Hydra Jet-Trooper" [Hydra] [Star] HydraAssault 2)
    { cdKeywords = singleton Quickstrike
    }

-- | Spider-Woman's nemesis set's Hydra Regular, two of them in the Hydra Patrol set.
hydraRegularHydraPatrol :: CardDef
hydraRegularHydraPatrol =
  (reprint "04152" hydraRegular)
    { cdEncounterSet = Just HydraPatrol
    , cdEncounterSetQuantity = Just 2
    }

-- | The core set's Hydra Soldier, three of them in the Hydra Patrol set.
hydraSoldierHydraPatrol :: CardDef
hydraSoldierHydraPatrol =
  (reprint "04153" hydraSoldier)
    { cdEncounterSet = Just HydraPatrol
    , cdEncounterSetQuantity = Just 3
    }
-- The Rise of Red Skull
-- Zola

ultimateBioServant :: CardDef
ultimateBioServant =
  (minion "04114" "Ultimate Bio-Servant" [Hydra, Mutate] [Star] Zola 4)
    { cdKeywords = singleton Toughness
    }

zolasMutate :: CardDef
zolasMutate = minion "04115" "Zola's Mutate" [Hydra, Mutate] [Star] Zola 3

berserkMutate :: CardDef
berserkMutate =
  (minion "04116" "Berserk Mutate" [Hydra, Mutate] [Star] Zola 3)
    { cdKeywords = singleton Quickstrike
    }

-- Red Skull

theSleeper :: CardDef
theSleeper =
  unique $
    (minion "04130" "The Sleeper" [Elite, Hydra, Robot] [Boost] RedSkull 1)
      { cdKeywords = fromList [Guard, Retaliate 1, Toughness]
      }

hydraExoSoldier :: CardDef
hydraExoSoldier =
  (minion "04131" "Hydra Exo-Soldier" [Hydra] [Star] RedSkull 3)
    { cdKeywords = singleton Toughness
    }

-- The Green Goblin

hiredGun :: CardDef
hiredGun = minion "02007" "Hired Gun" [Criminal] [Boost, Boost, Star] RiskyBusiness 3

privateSecuritySpecialist :: CardDef
privateSecuritySpecialist =
  (minion "02008" "Private Security Specialist" [Mercenary] [Boost, Star] RiskyBusiness 4)
    { cdKeywords = singleton Guard
    }

goblinKnight :: CardDef
goblinKnight = unique $ minion "02022" "Goblin Knight" [Elite, Goblin] [Boost, Boost, Star] MutagenFormula 1

goblinSoldier :: CardDef
goblinSoldier = minion "02023" "Goblin Soldier" [Goblin] [Star] MutagenFormula 4

goblinThrall :: CardDef
goblinThrall =
  (minion "02024" "Goblin Thrall" [Goblin] [Star] MutagenFormula 6)
    { cdKeywords = singleton Guard
    }

monster :: CardDef
monster = unique $ minion "02025" "Monster" [Elite, Goblin] [Boost, Boost, Star] MutagenFormula 1
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

macrobots :: CardDef
macrobots =
  (minion "11017" "Macrobots" [Robot, Trait.Temporal] [Star] Kang 3)
    { cdKeywords = fromList [Guard, Retaliate 1]
    }

-- The Rise of Red Skull

-- Taskmaster

hydraHunter :: CardDef
hydraHunter = minion "04101" "Hydra Hunter" [Hydra] [Star] Taskmaster 2

-- The Wrecking Crew

corruptPrisonGuard :: CardDef
corruptPrisonGuard = (minion "07008" "Corrupt Prison Guard" [Traitor] [] Wrecker 1) {cdKeywords = singleton Guard}

-- | Corrupt Prison Guard, one in each of the Wrecking Crew's decks.
corruptPrisonGuardThunderball :: CardDef
corruptPrisonGuardThunderball = (reprint "07023" corruptPrisonGuard) {cdEncounterSet = Just Thunderball}

corruptPrisonGuardPiledriver :: CardDef
corruptPrisonGuardPiledriver = (reprint "07037" corruptPrisonGuard) {cdEncounterSet = Just Piledriver}

corruptPrisonGuardBulldozer :: CardDef
corruptPrisonGuardBulldozer = (reprint "07052" corruptPrisonGuard) {cdEncounterSet = Just Bulldozer}

escapedConvict :: CardDef
escapedConvict = (minion "07009" "Escaped Convict" [Criminal] [Star] Wrecker 1) {cdKeywords = singleton Surge}

-- | Escaped Convict, one in each of the Wrecking Crew's decks.
escapedConvictThunderball :: CardDef
escapedConvictThunderball = (reprint "07024" escapedConvict) {cdEncounterSet = Just Thunderball}

escapedConvictPiledriver :: CardDef
escapedConvictPiledriver = (reprint "07038" escapedConvict) {cdEncounterSet = Just Piledriver}

escapedConvictBulldozer :: CardDef
escapedConvictBulldozer = (reprint "07053" escapedConvict) {cdEncounterSet = Just Bulldozer}

-- Ant-Man

yellowjacket :: CardDef
yellowjacket = unique $ minion "12027" "Yellowjacket" [Criminal] [Boost, Boost] AntManNemesis 1

-- Wasp

beetle :: CardDef
beetle =
  unique (minion "13028" "Beetle" [Criminal] [Boost, Boost] WaspNemesis 1)
    { cdKeywords = singleton Guard
    }

-- Quicksilver

avalanche :: CardDef
avalanche =
  unique (minion "14026" "Avalanche" [BrotherhoodOfMutants] [Boost, Boost] QuicksilverNemesis 1)
    { cdKeywords = singleton (Incite 2)
    }

-- Scarlet Witch

luminous :: CardDef
luminous = unique (minion "15025" "Luminous" [Elite] [Boost, Boost] ScarletWitchNemesis 1)

-- Star-Lord

misterKnife :: CardDef
misterKnife =
  unique $
    (minion "17026" "Mister Knife" [Criminal, Elite] [Boost, Boost] StarLordNemesis 1)
      { cdKeywords = singleton (Retaliate 1)
      }

-- Gamora

nebulaMinion :: CardDef
nebulaMinion =
  unique $
    (minion "18026" "Nebula" [Elite, Scoundrel] [Boost, Boost, Boost] GamoraNemesis 1)
      { cdKeywords = singleton (Retaliate 2)
      }

-- Groot

furnax :: CardDef
furnax = unique $ minion "16027" "Furnax" [Leviathon] [Boost, Boost, Boost] GrootNemesis 1

-- Rocket Raccoon

blackjackOHare :: CardDef
blackjackOHare =
  unique $
    (minion "16055" "Blackjack O'Hare" [Criminal] [Boost, Boost, Boost] RocketRaccoonNemesis 1)
      { cdKeywords = fromList [Quickstrike, Villainous]
      }

-- The Galaxy's Most Wanted: Badoon Headhunter

badoonHeadhunter :: CardDef
badoonHeadhunter = (minion "16183" "Badoon Headhunter" [Badoon] [Star] BadoonHeadhunter 1) {cdKeywords = fromList [Victory 2, Villainous]}

headhuntersHenchman :: CardDef
headhuntersHenchman = (minion "16186" "Headhunter's Henchman" [Badoon] [Boost, Boost, Boost] BadoonHeadhunter 1) {cdKeywords = fromList [Surge, Patrol]}

-- The Galaxy's Most Wanted: Brotherhood of Badoon

badoonEngineer :: CardDef
badoonEngineer = minion "16065" "Badoon Engineer" [Badoon] [Star] BrotherhoodOfBadoon 2

-- The Galaxy's Most Wanted: Band of Badoon

badoonAssassin :: CardDef
badoonAssassin = minion "16117" "Badoon Assassin" [Badoon] [Star] BandOfBadoon 2

badoonGrunt :: CardDef
badoonGrunt = minion "16118" "Badoon Grunt" [Badoon] [Star] BandOfBadoon 3

badoonLieutenant :: CardDef
badoonLieutenant = (minion "16119" "Badoon Lieutenant" [Badoon] [Boost, Star] BandOfBadoon 1) {cdKeywords = singleton Patrol}

badoonSentry :: CardDef
badoonSentry = (minion "16120" "Badoon Sentry" [Badoon] [Boost, Star] BandOfBadoon 2) {cdKeywords = singleton (Retaliate 1)}

badoonWarlord :: CardDef
badoonWarlord = minion "16121" "Badoon Warlord" [Badoon] [Star] BandOfBadoon 2

-- Kree Militants

kreeCommando :: CardDef
kreeCommando = (minion "16132" "Kree Commando" [Kree, Soldier] [Boost, Boost, Star] KreeMilitants 3) {cdKeywords = singleton Patrol}

kreeLieutenant :: CardDef
kreeLieutenant = (minion "16133" "Kree Lieutenant" [Kree, Soldier] [Star] KreeMilitants 2) {cdKeywords = fromList [Guard, Stalwart]}

kreePrivate :: CardDef
kreePrivate = (minion "16134" "Kree Private" [Kree, Soldier] [Boost, Star] KreeMilitants 3) {cdKeywords = singleton Quickstrike}

-- The Galaxy's Most Wanted: Space Pirates

pirateCommander :: CardDef
pirateCommander = (minion "16138" "Pirate Commander" [Criminal] [Boost, Star] SpacePirates 1) {cdKeywords = singleton Quickstrike}

pirateLackey :: CardDef
pirateLackey = (minion "16139" "Pirate Lackey" [Criminal] [Boost, Star] SpacePirates 4) {cdKeywords = singleton Quickstrike}

-- The Galaxy's Most Wanted: Infiltrate the Museum

-- | Printed "Monark Starstalker" (MarvelCDB: "Monarch").
monarkStarstalker :: CardDef
monarkStarstalker = unique $ (minion "16075" "Monark Starstalker" [BountyHunter] [Boost, Boost, Boost] InfiltrateTheMuseum 1) {cdKeywords = singleton Villainous}

-- Menagerie Medley

psionicGhost :: CardDef
psionicGhost = minion "16135" "Psionic Ghost" [Ghost] [Star] MenagerieMedley 4

servantBot :: CardDef
servantBot = (minion "16136" "Servant Bot" [Robot] [] MenagerieMedley 3) {cdKeywords = fromList [Guard, Patrol]}

starshark :: CardDef
starshark = (minion "16137" "Starshark" [Creature, Elite] [Star] MenagerieMedley 2) {cdKeywords = singleton Quickstrike}
-- Drax

-- boost icons from the printed card (MarvelCDB has none for this pack)
yotatTheDestroyer :: CardDef
yotatTheDestroyer =
  unique $
    (minion "19027" "Yotat the Destroyer" [Brute] [Boost, Boost, Boost] DraxNemesis 1)
      { cdKeywords = fromList [Guard, Retaliate 1]
      }

-- Venom

enragedSymbiote :: CardDef
enragedSymbiote =
  (minion "20025" "Enraged Symbiote" [Symbiote] [Star] VenomNemesis 4)
    { cdKeywords = fromList [Guard, Patrol]
    }

-- Spectrum

-- | Spectrum's nemesis minion, not the Masters of Evil one (01129).
radioactiveManNemesis :: CardDef
radioactiveManNemesis = unique $ minion "21027" "Radioactive Man" [Elite, Genius] [Star] SpectrumNemesis 1

-- Adam Warlock

theMagus :: CardDef
theMagus =
  unique $
    (minion "21067" "The Magus" [Elite, Mystic] [Boost, Boost] AdamWarlockNemesis 1)
      { cdKeywords = fromList [Quickstrike, Toughness]
      }

zealotOfTruth :: CardDef
zealotOfTruth = minion "21069" "Zealot of Truth" [Mystic] [Star] AdamWarlockNemesis 2

-- The Mad Titan's Shadow: Tower Defense

blackOrderBesieger :: CardDef
blackOrderBesieger = minion "21102" "Black Order Besieger" [BlackOrder] [Boost] TowerDefense 4

-- The Mad Titan's Shadow: Black Order

blackDwarf :: CardDef
blackDwarf = unique $ minion "21085" "Black Dwarf" [BlackOrder, Elite] [Boost, Boost, Boost] BlackOrderSet 1

supergiant :: CardDef
supergiant = unique $ (minion "21086" "Supergiant" [BlackOrder, Elite] [Boost, Boost] BlackOrderSet 1) {cdKeywords = singleton Quickstrike}

-- Armies of Titan

blackOrderInfantry :: CardDef
blackOrderInfantry = (minion "21089" "Black Order Infantry" [BlackOrder] [Star] ArmiesOfTitan 2) {cdKeywords = singleton Guard}

outrider :: CardDef
outrider = minion "21090" "Outrider" [BlackOrder] [Star] ArmiesOfTitan 2

-- The Mad Titan's Shadow: Children of Thanos (the villains of the same names are Tower
-- Defense's and Ebony Maw's)

corvusGlaiveMinion :: CardDef
corvusGlaiveMinion = unique $ (minion "21125" "Corvus Glaive" [BlackOrder, Elite] [Star] ChildrenOfThanos 1) {cdKeywords = fromList [Retaliate 1, Toughness]}

proximaMidnightMinion :: CardDef
proximaMidnightMinion = unique $ minion "21126" "Proxima Midnight" [BlackOrder, Elite] [Star] ChildrenOfThanos 1

ebonyMawMinion :: CardDef
ebonyMawMinion = unique $ (minion "21127" "Ebony Maw" [BlackOrder, Elite] [Boost, Star] ChildrenOfThanos 1) {cdKeywords = singleton Villainous}

-- The Mad Titan's Shadow: Hela

garm :: CardDef
garm = unique $ (minion "21143" "Garm" [Creature, Elite] [Boost, Boost] Hela 1) {cdKeywords = fromList [Toughness, Victory 2]}

skurge :: CardDef
skurge = unique $ (minion "21144" "Skurge" [Asgard, Elite] [Boost, Boost, Boost] Hela 1) {cdKeywords = fromList [Toughness, Victory 3]}

nidhogg :: CardDef
nidhogg = unique $ (minion "21145" "Nidhogg" [Creature, Elite] [Boost, Boost, Boost, Boost] Hela 1) {cdKeywords = fromList [Toughness, Victory 4]}

-- Legions of Hel

draugr :: CardDef
draugr = (minion "21152" "Draugr" [Undead] [Boost] LegionsOfHel 2) {cdKeywords = singleton Guard}

-- | An ally treated as a minion with a blank text box (Fallen Warrior, Beguiled): the
-- entity it is, its card the ally's.
allyMinion :: CardDef
allyMinion = minion "xallyminion" "Ally" [] [] LegionsOfHel 0

-- Frost Giants

laufey :: CardDef
laufey = unique $ (minion "21156" "Laufey" [Elite, Giant] [Boost, Boost, Boost, Boost] FrostGiants 1) {cdKeywords = singleton Toughness}

-- | The same card as Thor's nemesis set's Frost Giant.
frostGiantFrostGiants :: CardDef
frostGiantFrostGiants = (reprint "21157" frostGiant) {cdEncounterSet = Just FrostGiants, cdEncounterSetQuantity = Just 2}

-- Enchantress

enchantress :: CardDef
enchantress = unique $ minion "21177" "Enchantress" [Asgard, Elite] [Boost, Boost] Enchantress 1

-- The Mad Titan's Shadow: the campaign set

-- | Save the Shawarma Place's other side.
blackSwan :: CardDef
blackSwan = unique $ (minion "21182b" "Black Swan" [BlackOrder, Elite] [] MadTitansShadowCampaign 1) {cdKeywords = fromList [Toughness, Victory 2]}

-- Nebula

gamoraNemesis :: CardDef
gamoraNemesis = (minion "22028" "Gamora" [Elite, Outlaw] [Boost, Boost, Boost] NebulaNemesis 1)
    { cdUnique = True
    }

-- War Machine

livingLaser :: CardDef
livingLaser = (minion "23029" "Living Laser" [Criminal] [Boost, Boost] WarMachineNemesis 1)
    { cdUnique = True
    , cdKeywords = fromList [Quickstrike]
    }

-- Valkyrie

enchantressValkyrie :: CardDef
enchantressValkyrie = (reprint "25029" enchantress) {cdEncounterSet = Just ValkyrieNemesis, cdEncounterSetQuantity = Just 1}

-- Vision

ultronNemesis :: CardDef
ultronNemesis = (minion "26029" "Ultron" [Android, Elite] [Boost, Boost] VisionNemesis 1)
    { cdUnique = True
    , cdKeywords = fromList [Toughness]
    }


-- The Hood

madameMasque :: CardDef
madameMasque = unique $ (minion "24010" "Madame Masque" [Criminal, Trait.MastersOfEvil] [Boost, Boost, Boost] TheHood 1) {cdKeywords = singleton Guard}


-- Sinister Motives

theLizard :: CardDef
theLizard = (minion "27027" "The Lizard" [Brute, Creature] [Boost, Boost] GhostSpiderNemesis 1)
    { cdUnique = True
    }

prowler :: CardDef
prowler = (minion "27058" "Prowler" [Criminal] [Boost, Boost] SpiderManMoralesNemesis 1)
    { cdUnique = True
    , cdKeywords = fromList [Stalwart]
    }

-- Sinister Motives

smMinionDef :: CardCode -> Name -> SMSet.EncounterSet -> Natural -> CardDef
smMinionDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = MinionType
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

smSandClone :: CardDef
smSandClone = (smMinionDef "27067" "Sand Clone" SMSet.Sandman 4)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smShiftingApparition :: CardDef
smShiftingApparition = (smMinionDef "27091" "Shifting Apparition" SMSet.Mysterio 4)
  { cdTraits = fromList [SMTrait.Illusion]
  , cdKeywords = fromList [SMKeyword.Guard]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSymbioticBerserker :: CardDef
smSymbioticBerserker = (smMinionDef "27121" "Symbiotic Berserker" SMSet.VenomGoblin 2)
  { cdTraits = fromList [SMTrait.Symbiote]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSymbioticMonstrosity :: CardDef
smSymbioticMonstrosity = (smMinionDef "27122" "Symbiotic Monstrosity" SMSet.VenomGoblin 1)
  { cdTraits = fromList [SMTrait.Elite, SMTrait.Symbiote]
  , cdKeywords = fromList [SMKeyword.Retaliate 1, SMKeyword.Steady, SMKeyword.Toughness]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost, SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSymbioticThrall :: CardDef
smSymbioticThrall = (smMinionDef "27123" "Symbiotic Thrall" SMSet.VenomGoblin 4)
  { cdTraits = fromList [SMTrait.Symbiote]
  , cdKeywords = fromList [SMKeyword.Guard]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smRhino :: CardDef
smRhino = (smMinionDef "27128" "Rhino" SMSet.CityInChaos 1)
  { cdTraits = fromList [SMTrait.Criminal, SMTrait.Elite]
  , cdKeywords = fromList [SMKeyword.Steady]
  , cdUnique = True
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smCommonCriminal :: CardDef
smCommonCriminal = (smMinionDef "27131" "Common Criminal" SMSet.DownToEarth 3)
  { cdTraits = fromList [SMTrait.Criminal]
  , cdKeywords = fromList [SMKeyword.Surge]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smLifeSizeDecoy :: CardDef
smLifeSizeDecoy = (smMinionDef "27142" "Life-Size Decoy" SMSet.GuerrillaTactics 2)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smEvilDoppelganger :: CardDef
smEvilDoppelganger = (smMinionDef "27154" "Evil Doppelgänger" SMSet.PersonalNightmare 2)
  { cdTraits = fromList [SMTrait.Illusion]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smDoctorOctopus :: CardDef
smDoctorOctopus = (smMinionDef "27158" "Doctor Octopus" SMSet.SinisterAssault 1)
  { cdTraits = fromList [SMTrait.Elite, SMTrait.Genius]
  , cdKeywords = fromList [SMKeyword.Incite 2, SMKeyword.Villainous]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smElectro :: CardDef
smElectro = (smMinionDef "27159" "Electro" SMSet.SinisterAssault 1)
  { cdTraits = fromList [SMTrait.Criminal, SMTrait.Elite]
  , cdKeywords = fromList [SMKeyword.Retaliate 1, SMKeyword.Villainous]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smHobgoblin :: CardDef
smHobgoblin = (smMinionDef "27160" "Hobgoblin" SMSet.SinisterAssault 1)
  { cdTraits = fromList [SMTrait.Aerial, SMTrait.Elite]
  , cdKeywords = fromList [SMKeyword.Patrol, SMKeyword.Villainous]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smKravenTheHunter :: CardDef
smKravenTheHunter = (smMinionDef "27161" "Kraven the Hunter" SMSet.SinisterAssault 1)
  { cdTraits = fromList [SMTrait.Elite]
  , cdKeywords = fromList [SMKeyword.Steady, SMKeyword.Villainous]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smScorpion :: CardDef
smScorpion = (smMinionDef "27162" "Scorpion" SMSet.SinisterAssault 1)
  { cdTraits = fromList [SMTrait.Brute, SMTrait.Elite]
  , cdKeywords = fromList [SMKeyword.Toughness, SMKeyword.Villainous]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smVulture :: CardDef
smVulture = (smMinionDef "27163" "Vulture" SMSet.SinisterAssault 1)
  { cdTraits = fromList [SMTrait.Aerial, SMTrait.Elite]
  , cdKeywords = fromList [SMKeyword.Quickstrike, SMKeyword.Villainous]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smEnragedSymbiote :: CardDef
smEnragedSymbiote = (smMinionDef "27167" "Enraged Symbiote" SMSet.SymbioticStrength 3)
  { cdTraits = fromList [SMTrait.Symbiote]
  , cdKeywords = fromList [SMKeyword.Guard, SMKeyword.Patrol]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

-- Nova

warbringer :: CardDef
warbringer = (minion "28023" "Warbringer" [Brute, Chitauri] [Boost, Boost, Boost] SMSet.NovaNemesis 1)
  { cdUnique = True
  }

armadillo :: CardDef
armadillo = (minion "28029" "Armadillo" [Brute, Elite] [Boost, Boost, Boost] SMSet.Armadillo 1)
  { cdUnique = True
  , cdKeywords = fromList [Toughness]
  }


-- Ironheart

luciaVonBardas :: CardDef
luciaVonBardas = (minion "29030" "Lucia von Bardas" [Criminal, Cyborg, Elite] [Boost, Boost, Boost] SMSet.IronheartNemesis 1)
  { cdUnique = True
  }

zzzax :: CardDef
zzzax = (minion "29037" "Zzzax" [Criminal] [Star] SMSet.ZzzaxSet 1)
  { cdUnique = True
  }


-- Spider-Ham

theGreenGobbler :: CardDef
theGreenGobbler = (minion "30026" "The Green Gobbler" [Cartoon, Criminal] [Boost, Boost, Boost] SMSet.SpiderHamNemesis 1)
  { cdUnique = True
  }

bora :: CardDef
bora = (minion "30031" "Bora" [Inheritor] [Boost, Boost] SMSet.Inheritors 1)
  { cdUnique = True
  }

brix :: CardDef
brix = (minion "30032" "Brix" [Inheritor] [Boost, Boost] SMSet.Inheritors 1)
  { cdUnique = True
  }

daemos :: CardDef
daemos = (minion "30033" "Daemos" [Inheritor] [Boost, Boost] SMSet.Inheritors 1)
  { cdUnique = True
  }

jennix :: CardDef
jennix = (minion "30034" "Jennix" [Inheritor] [Boost, Boost] SMSet.Inheritors 1)
  { cdUnique = True
  }

karn :: CardDef
karn = (minion "30035" "Karn" [Inheritor] [Boost, Boost] SMSet.Inheritors 1)
  { cdUnique = True
  }

morlun :: CardDef
morlun = (minion "30036" "Morlun" [Elite, Inheritor] [Boost, Boost] SMSet.Inheritors 1)
  { cdUnique = True
  }

solus :: CardDef
solus = (minion "30037" "Solus" [Elite, Inheritor] [Boost, Boost, Boost] SMSet.Inheritors 1)
  { cdUnique = True
  }

verna :: CardDef
verna = (minion "30038" "Verna" [Elite, Inheritor] [Boost, Boost] SMSet.Inheritors 1)
  { cdUnique = True
  }


-- SP//dr

morbius :: CardDef
morbius = (minion "31027" "M.O.R.B.I.U.S." [Creature] [Boost, Boost] SpdrSet.SpdrNemesis 1)
  { cdUnique = False
  }

bombshellSinister :: CardDef
bombshellSinister = (minion "31031" "Bombshell" [Criminal] [Star] SpdrSet.IronSpiderSinisterSix 1)
  { cdUnique = True
  }

electroSinister :: CardDef
electroSinister = (minion "31032" "Electro" [Criminal] [Boost] SpdrSet.IronSpiderSinisterSix 1)
  { cdUnique = True
  }

hobgoblinSinister :: CardDef
hobgoblinSinister = (minion "31033" "Hobgoblin" [Criminal] [Boost, Boost] SpdrSet.IronSpiderSinisterSix 1)
  { cdUnique = True
  }

ironSpider :: CardDef
ironSpider = (minion "31034" "Iron Spider" [Criminal, Elite] [Boost, Boost, Boost] SpdrSet.IronSpiderSinisterSix 1)
  { cdUnique = True
  , cdKeywords = fromList [SpdrKeyword.Guard, SpdrKeyword.Patrol, SpdrKeyword.Retaliate 1, SpdrKeyword.Toughness]
  }

sandmanSinister :: CardDef
sandmanSinister = (minion "31035" "Sandman" [Criminal] [Boost, Star] SpdrSet.IronSpiderSinisterSix 1)
  { cdUnique = True
  }

spot :: CardDef
spot = (minion "31036" "Spot" [Criminal] [Star] SpdrSet.IronSpiderSinisterSix 1)
  { cdUnique = True
  }

-- Mutant Genesis

juggernaut :: CardDef
juggernaut = (minion "32026" "Juggernaut" [Brute, Elite] [Boost, Boost, Star] ColossusNemesis 1)
  { cdUnique = True
  , cdKeywords = fromList [Toughness, Stalwart]
  }

-- Mutant Genesis

whiteQueenMutantGenesis :: CardDef
whiteQueenMutantGenesis = (minion "32056" "White Queen" [Elite, Psionic] [Boost, Star] ShadowcatNemesis 1)
  { cdUnique = True
  , cdKeywords = fromList [Villainous]
  }

-- Mutant Genesis

hellfirePawn :: CardDef
hellfirePawn = (minion "32058" "Hellfire Pawn" [Hellfire] [Star] ShadowcatNemesis 2)
  { cdKeywords = fromList [Guard, Patrol, Surge]
  }

-- Mutant Genesis scenario definitions
mgAvalanche :: CardDef
mgAvalanche = (smMinionDef "32073" ("Avalanche") MGSet.Brotherhood 1)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Star]
  , cdKeywords = fromList []
  }

mgBlob :: CardDef
mgBlob = (smMinionDef "32074" ("Blob") MGSet.Brotherhood 1)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Guard]
  }

mgPyro :: CardDef
mgPyro = (smMinionDef "32075" ("Pyro") MGSet.Brotherhood 1)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgToad :: CardDef
mgToad = (smMinionDef "32076" ("Toad") MGSet.Brotherhood 1)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Star]
  , cdKeywords = fromList []
  }

mgMystique :: CardDef
mgMystique = (smMinionDef "32080" ("Mystique") MGSet.Mystique 1)
  { cdTraits = fromList [MGTrait.BrotherhoodOfMutants, MGTrait.Elite]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Toughness]
  }

mgSentinelMarkIv :: CardDef
mgSentinelMarkIv = (smMinionDef "32093" ("Sentinel Mark IV") MGSet.ProjectWideawake 2)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList [MGKeyword.Guard, MGKeyword.Patrol]
  }

mgSentinelMarkIi :: CardDef
mgSentinelMarkIi = (smMinionDef "32101" ("Sentinel Mark II") MGSet.ZeroTolerance 2)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgSentinelMarkIii :: CardDef
mgSentinelMarkIii = (smMinionDef "32102" ("Sentinel Mark III") MGSet.ZeroTolerance 2)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList [MGKeyword.Toughness]
  }

mgSentinelMarkV :: CardDef
mgSentinelMarkV = (smMinionDef "32105" ("Sentinel Mark V") MGSet.Sentinels 2)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgSentinelMarkVi :: CardDef
mgSentinelMarkVi = (smMinionDef "32106" ("Sentinel Mark VI") MGSet.Sentinels 2)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList [MGKeyword.Quickstrike]
  }

mgSentinelMarkViii :: CardDef
mgSentinelMarkViii = (smMinionDef "32114" ("Sentinel Mark VIII") MGSet.MasterMold 2)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgMTypeSentinel :: CardDef
mgMTypeSentinel = (smMinionDef "32146" ("M-Type Sentinel") MGSet.Magneto 4)
  { cdTraits = fromList [MGTrait.Sentinel]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList [MGKeyword.Guard]
  }

mgFabianCortez :: CardDef
mgFabianCortez = (smMinionDef "32159" ("Fabian Cortez") MGSet.Acolytes 1)
  { cdTraits = fromList [MGTrait.Acolyte, MGTrait.Elite]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Villainous, MGKeyword.Teamwork MGTrait.Acolyte]
  }

mgAmeliaVoght :: CardDef
mgAmeliaVoght = (smMinionDef "32160" ("Amelia Voght") MGSet.Acolytes 1)
  { cdTraits = fromList [MGTrait.Acolyte]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Stalwart, MGKeyword.Teamwork MGTrait.Acolyte]
  }

mgSenyaka :: CardDef
mgSenyaka = (smMinionDef "32161" ("Senyaka") MGSet.Acolytes 1)
  { cdTraits = fromList [MGTrait.Acolyte]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Teamwork MGTrait.Acolyte]
  }

mgDelgado :: CardDef
mgDelgado = (smMinionDef "32162" ("Delgado") MGSet.Acolytes 1)
  { cdTraits = fromList [MGTrait.Acolyte]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Retaliate 1, MGKeyword.Teamwork MGTrait.Acolyte]
  }

mgUnuscione :: CardDef
mgUnuscione = (smMinionDef "32163" ("Unuscione") MGSet.Acolytes 1)
  { cdTraits = fromList [MGTrait.Acolyte]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Teamwork MGTrait.Acolyte]
  }

mgNimrod :: CardDef
mgNimrod = (smMinionDef "32166" ("Nimrod") MGSet.FuturePast 1)
  { cdTraits = fromList [MGTrait.Elite, MGTrait.Sentinel]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Stalwart, MGKeyword.Victory 1]
  }

mgBastion :: CardDef
mgBastion = (smMinionDef "32167" ("Bastion") MGSet.FuturePast 1)
  { cdTraits = fromList [MGTrait.Elite, MGTrait.Sentinel]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList [MGKeyword.Toughness, MGKeyword.Villainous, MGKeyword.Victory 1]
  }
