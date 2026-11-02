module Marvel.Minion.Cards where

import Marvel.AgentsOfShield.EncounterCards qualified as AosEncounter
import Marvel.FalconWinter.Cards qualified as FalconWinter

import Marvel.Ability.Types qualified as AospAbility ()
import Marvel.Aspect qualified as AospAspect ()
import Marvel.Boost qualified as AospBoost
import Marvel.Card.Def qualified as AospCard
import Marvel.Cost.Types qualified as AospCost
import Marvel.Criteria qualified as AospCriteria
import Marvel.EncounterSet qualified as AospSet
import Marvel.Keyword qualified as AospKeyword
import Marvel.Matchers qualified as AospMatch ()
import Marvel.Name qualified as AospName ()
import Marvel.Resource.Types qualified as AospResource ()
import Marvel.Trait qualified as AospTrait
import Marvel.Window.Types qualified as AospWindow ()

import Marvel.BlackPantherShuri.Cards qualified as BlackPantherShuriPack
import Marvel.Silk.Cards qualified as SilkPack

import Marvel.TricksterTakeover.Cards qualified as TT

import Marvel.Window.Types qualified as NcWindow ()

import Marvel.Trait.Types qualified as NcTrait

import Marvel.Resource.Types qualified as NcResource ()

import Marvel.Name qualified as NcName ()

import Marvel.Matchers qualified as NcMatch ()

import Marvel.Keyword qualified as NcKeyword

import Marvel.EncounterSet qualified as NcSet

import Marvel.Criteria qualified as NcCriteria

import Marvel.Cost.Types qualified as NcCost

import Marvel.Card.Def qualified as NcCard ()

import Marvel.Boost qualified as NcBoost

import Marvel.Aspect qualified as NcAspect ()

import Marvel.Ability.Types qualified as NcAbility ()

import Marvel.Jubilee.Cards qualified as Jubilee

import Marvel.Iceman.Cards qualified as IcemanPack
import Marvel.Ability.Types qualified as MgAbility ()
import Marvel.Aspect qualified as MgAspect ()
import Marvel.Boost qualified as MgBoost
import Marvel.Card.Def qualified as MgCard
import Marvel.Cost.Types qualified as MgCost
import Marvel.Criteria qualified as MgCriteria
import Marvel.EncounterSet qualified as MgSet
import Marvel.Keyword qualified as MgKeyword
import Marvel.Matchers qualified as MgMatch ()
import Marvel.Name qualified as MgName ()
import Marvel.Resource.Types qualified as MgResource ()
import Marvel.Trait qualified as MgTrait
import Marvel.Window.Types qualified as MgWindow ()




import Marvel.Boost qualified as AoaMB
import Marvel.EncounterSet qualified as AoaMS
import Marvel.Keyword qualified as AoaMK
import Marvel.Trait qualified as AoaMT
import Marvel.Boost qualified as BpBoost
import Marvel.Card.Def qualified as BpCard
import Marvel.Cost.Types qualified as BpCost
import Marvel.Criteria qualified as BpCriteria
import Marvel.EncounterSet qualified as BpSet
import Marvel.Keyword qualified as BpKeyword
import Marvel.Trait qualified as BpTrait

import Marvel.Ability.Types qualified as DpAbility ()
import Marvel.Aspect qualified as DpAspect ()
import Marvel.Boost qualified as DpBoost
import Marvel.Card.Def qualified as DpCard
import Marvel.Cost.Types qualified as DpCost
import Marvel.Criteria qualified as DpCriteria
import Marvel.EncounterSet qualified as DpSet
import Marvel.Keyword qualified as DpKeyword
import Marvel.Matchers qualified as DpMatch ()
import Marvel.Name qualified as DpName
import Marvel.Resource.Types qualified as DpResource ()
import Marvel.Trait qualified as DpTrait
import Marvel.Window.Types qualified as DpWindow ()

import Marvel.Ability.Types qualified as AnAbility ()
import Marvel.Aspect qualified as AnAspect ()
import Marvel.Boost qualified as AnBoost
import Marvel.Card.Def qualified as AnCard
import Marvel.Cost.Types qualified as AnCost
import Marvel.Criteria qualified as AnCriteria
import Marvel.EncounterSet qualified as AnSet
import Marvel.Keyword qualified as AnKeyword ()
import Marvel.Matchers qualified as AnMatch ()
import Marvel.Name qualified as AnName ()
import Marvel.Resource.Types qualified as AnResource ()
import Marvel.Trait qualified as AnTrait
import Marvel.Window.Types qualified as AnWindow ()

import Marvel.Ability.Types qualified as PsyAbility ()
import Marvel.Aspect qualified as PsyAspect ()
import Marvel.Boost qualified as PsyBoost
import Marvel.Card.Def qualified as PsyCard
import Marvel.Cost.Types qualified as PsyCost
import Marvel.Criteria qualified as PsyCriteria
import Marvel.EncounterSet qualified as PsySet
import Marvel.Keyword qualified as PsyKeyword ()
import Marvel.Matchers qualified as PsyMatch ()
import Marvel.Name qualified as PsyName ()
import Marvel.Resource.Types qualified as PsyResource ()
import Marvel.Trait qualified as PsyTrait
import Marvel.Window.Types qualified as PsyWindow ()

import Marvel.Hood.Cards (hoodCardDef)
import Marvel.EncounterSet qualified as HoodSet
import Marvel.Trait qualified as HoodTrait
import Marvel.Keyword qualified as HoodKeyword
import Marvel.Boost qualified as HoodBoost
import Marvel.GameValue.Types qualified as HoodValue ()



import Marvel.Resource.Types qualified as NEResource ()

import Marvel.Boost qualified as NEBoost

import Marvel.GameValue.Types qualified as NEValue ()

import Marvel.EncounterSet qualified as NESet

import Marvel.Keyword qualified as NEKeyword

import Marvel.Trait qualified as NETrait

import Marvel.Aspect qualified as NEAspect ()

import Marvel.Boost qualified as NEB
import Marvel.EncounterSet qualified as NES
import Marvel.Aspect qualified as NEA ()
import Marvel.Resource.Types qualified as NER ()
import Marvel.Keyword qualified as NEK ()
import Marvel.Trait qualified as NET
import Marvel.Trait qualified as NE
import Marvel.Keyword qualified as NE
import Marvel.Boost qualified as NE
import Marvel.Ability.Types qualified as NEAbility ()

import Marvel.Trait qualified as MojoTrait
import Marvel.EncounterSet qualified as MojoSet
import Marvel.Keyword qualified as MojoKeyword
import Marvel.GameValue.Types qualified as MojoValue ()
import Marvel.Boost qualified as MojoBoost

import Marvel.Keyword qualified as SpdrKeyword
import Marvel.Resource ()
import Marvel.EncounterSet qualified as SpdrSet
import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait
import Marvel.Keyword qualified as SMKeyword
import Marvel.GameValue.Types qualified as SMValue ()
import Marvel.Boost qualified as SMBoost

import Marvel.Cost.Types
import Marvel.Window ()
import Marvel.Matchers ()
import Marvel.Trait qualified as MGTrait
import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.GameValue.Types qualified as MGValue ()
import Marvel.Boost qualified as MGBoost
import Marvel.Prelude


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
      [ TT.tt55017, TT.tt55018, TT.tt55019, TT.tt55020, TT.tt55037, TT.tt55038, TT.tt55039, TT.tt55040, TT.tt55041, TT.tt55042, TT.tt55043, TT.tt55044, TT.tt55056, TT.tt55057, TT.tt55058, TT.tt55059
  , aos50072, aos50073, aos50080, aos50083, aos50084, aos50093, aos50094, aos50098, aos50099, aos50100, aos50113, aos50120, aos50125, aos50126, aos50133, aos50139, aos50143, aos50148, aos50152, aos50156, aos50161, aos50172, aos50178
  , FalconWinter.fw53030, FalconWinter.fw53032, FalconWinter.fw53038, FalconWinter.fw54028, FalconWinter.fw54031, FalconWinter.fw54034
  , aosP50030, aosP50060, aosP50062
  , BlackPantherShuriPack.klawShuri, BlackPantherShuriPack.mUSICShuri, BlackPantherShuriPack.joystickShuri, SilkPack.morlunSilk, SilkPack.atlasSilk
  , nc48027, nc48034, nc48035, nc48036, nc48037
  , Jubilee.jubilee47024, Jubilee.jubilee47030
  , pyroIceman
  , sauron
  , exodusMagneto, fabianCortez, frenzy, sebastianShaw, selene, hellfirePawnMagneto
  , infiniteHunter, infiniteSoldier, ahab, hound, gauntlet, barrage, hardDrive, tusk, psynapse, pterosaur, velociraptor, giantApe, magistrate, armoredUnibike, genoshanMech, gladiator, oracle, manta, earthquake, warstar, ozymandias, scarab, clanAkkabaZealot, agentOfApocalypse, misterSinisterMinion, misterSinisterOverseer, theShadowKingMinion, theShadowKingOverseer, abyssMinion, abyssOverseer, sugarManMinion, sugarManOverseer, mikhailRasputinMinion, mikhailRasputinOverseer
  , witchfire
  , sym
  , belasco
  , trevorFitzroy, bantam
  , butler, mutatedSoldier, dreadpool
  , ladyDeathstrikeX23
  , harpoon
  , chimera
  , hood24015, hood24016, hood24018, hood24024, hood24025, hood24026, hood24027, hood24034, hood24035, hood24041, hood24043, hood24044, hood24045, hood24046, hood24047, hood24065, hood24066, hood24067, hood24068
      , ne40094, ne40095, ne40096, ne40097, ne40098, ne40099, ne40100, ne40112, ne40113, ne40114, ne40115, ne40116, ne40132, ne40133, ne40145, ne40174, ne40180, ne40181, ne40182, ne40183, ne40185, ne40186, ne40187, ne40188, ne40198, ne40199, ne40200, ne40201
  , prototype
  , topaz
  , cable40032
  , mojo39007, mojo39029, mojo39042, mojo39043, mojo39044, mojo39049, mojo39050, mojo39051, mojo39052, mojo39054, mojo39055, mojo39056, mojo39057, mojo39058, mojo39068, mojo39069
  , rogueMystique, donaldPierce, skullbuster, bonebreaker, wadeCole, murrayReese
  , belladonna, guildAssassin, exodus
  , callisto, theShadowKing
  , misterSinister
  , omegaRed, ladyDeathstrike
  , mgAvalanche, mgBlob, mgPyro, mgToad, mgMystique, mgSentinelMarkIv, mgSentinelMarkIi, mgSentinelMarkIii, mgSentinelMarkV, mgSentinelMarkVi, mgSentinelMarkViii, mgMTypeSentinel, mgFabianCortez, mgAmeliaVoght, mgSenyaka, mgDelgado, mgUnuscione, mgNimrod, mgBastion, smSandClone, smShiftingApparition, smSymbioticBerserker, smSymbioticMonstrosity, smSymbioticThrall, smRhino, smCommonCriminal, smLifeSizeDecoy, smEvilDoppelganger, smDoctorOctopus, smElectro, smHobgoblin, smKravenTheHunter, smScorpion, smVulture, smEnragedSymbiote
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
  , darkPhoenix
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
designatedNemesisMinions = [exodusMagneto, pyroIceman, Jubilee.jubilee47024, nc48027, trevorFitzroy, belasco, belladonna, baronZemo, thomasEdison, loki, taskmaster, theMagus]

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

-- Wolverine

omegaRed :: CardDef
omegaRed = (minion "35028" "Omega Red" [Brute, Elite] [Boost, Boost, Boost] WolverineNemesis 1) {cdKeywords = fromList [Retaliate 1, Steady]}

ladyDeathstrike :: CardDef
ladyDeathstrike = (minion "35034" "Lady Deathstrike" [Elite, Trait.Reaver] [Boost, Boost, Boost] LadyDeathstrikeSet 1) {cdUnique = True, cdKeywords = fromList [Quickstrike]}


-- Phoenix

darkPhoenix :: CardDef
darkPhoenix = (minion "34029" "Dark Phoenix" [CosmicEntity, Elite] [Boost, Boost, Boost] PhoenixNemesis 1) {cdUnique = True, cdKeywords = fromList [Steady, Toughness, Villainous]}


-- Cyclops

misterSinister :: CardDef
misterSinister = (minion "33028" "Mister Sinister" [Elite, Marauder] [Boost, Boost, Star] CyclopsNemesis 1) { cdUnique = True, cdKeywords = fromList [Stalwart, Toughness, Villainous] }


-- Storm

callisto :: CardDef
callisto = (minion "36031" "Callisto" [Elite, Morlock] [Boost, Boost, Boost] StormNemesis 1) { cdUnique = True, cdKeywords = fromList [Quickstrike] }

theShadowKing :: CardDef
theShadowKing = (minion "36036" "The Shadow King" [Elite, Psionic] [Boost, Boost, Boost] ShadowKingSet 1) { cdUnique = True }

-- Gambit

belladonna :: CardDef
belladonna = (minion "37026" "Belladonna" [Assassin, Elite] [Boost, Boost, Boost] GambitNemesisSet 1) { cdUnique = True, cdKeywords = fromList [Quickstrike, Toughness] }

guildAssassin :: CardDef
guildAssassin = (minion "37028" "Guild Assassin" [Assassin] [Boost, Boost] GambitNemesisSet 2) { cdKeywords = fromList [Quickstrike] }

exodus :: CardDef
exodus = (minion "37032" "Exodus" [Acolyte, Elite, Psionic] [Boost, Boost, Boost] ExodusSet 1) { cdUnique = True, cdKeywords = fromList [Retaliate 1, Villainous] }


-- Rogue

rogueMystique :: CardDef
rogueMystique = (minion "38025" "Mystique" [BrotherhoodOfMutants, Elite] [Boost, Boost, Boost] RogueNemesisSet 1) { cdUnique = True, cdKeywords = fromList [Toughness, Villainous] }

donaldPierce :: CardDef
donaldPierce = (minion "38029" "Donald Pierce" [Elite, Trait.Reaver] [Boost, Boost, Boost] ReaversSet 1) { cdUnique = True, cdKeywords = fromList [Teamwork Trait.Reaver, Villainous] }

skullbuster :: CardDef
skullbuster = (minion "38030" "Skullbuster" [Trait.Reaver] [Boost, Boost] ReaversSet 1) { cdUnique = True, cdKeywords = fromList [Teamwork Trait.Reaver, Toughness] }

bonebreaker :: CardDef
bonebreaker = (minion "38031" "Bonebreaker" [Trait.Reaver] [Boost, Boost] ReaversSet 1) { cdUnique = True, cdKeywords = fromList [Teamwork Trait.Reaver, Toughness] }

wadeCole :: CardDef
wadeCole = (minion "38032" "Wade Cole" [Trait.Reaver] [Boost] ReaversSet 1) { cdUnique = True, cdKeywords = fromList [Teamwork Trait.Reaver] }

murrayReese :: CardDef
murrayReese = (minion "38033" "Murray Reese" [Trait.Reaver] [Boost] ReaversSet 1) { cdUnique = True, cdKeywords = fromList [Teamwork Trait.Reaver] }


-- Mojo Mania

mojoCardDef :: CardCode -> Name -> MojoSet.EncounterSet -> Natural -> CardDef
mojoCardDef code name set quantity = CardDef
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

mojo39007 :: CardDef
mojo39007 = (mojoCardDef "39007" "Surprise Contender" MojoSet.Magog 1)
  { cdTraits = fromList [MojoTrait.Clone, MojoTrait.Elite]
  , cdUnique = True
  , cdKeywords = fromList [MojoKeyword.Villainous]
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39029 :: CardDef
mojo39029 = (mojoCardDef "39029" "Supporting Actor" MojoSet.Mojo 2)
  { cdTraits = fromList [MojoTrait.Robot, MojoTrait.Thespian]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Star]
  }

mojo39042 :: CardDef
mojo39042 = (mojoCardDef "39042" "Dragon" MojoSet.Fantasy 1)
  { cdTraits = fromList [MojoTrait.Dragon, MojoTrait.Elite]
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Toughness, MojoKeyword.Retaliate (1)]
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39043 :: CardDef
mojo39043 = (mojoCardDef "39043" "Goblin" MojoSet.Fantasy 1)
  { cdTraits = fromList [MojoTrait.Goblin]
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Surge]
  , cdBoostIcons = []
  }

mojo39044 :: CardDef
mojo39044 = (mojoCardDef "39044" "Troll" MojoSet.Fantasy 1)
  { cdTraits = fromList [MojoTrait.Brute, MojoTrait.Troll]
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Toughness]
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39049 :: CardDef
mojo39049 = (mojoCardDef "39049" "Cultist" MojoSet.Horror 1)
  { cdTraits = fromList [MojoTrait.Mystic]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost]
  }

mojo39050 :: CardDef
mojo39050 = (mojoCardDef "39050" "The Kraken" MojoSet.Horror 1)
  { cdTraits = fromList [MojoTrait.Creature, MojoTrait.Elite]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39051 :: CardDef
mojo39051 = (mojoCardDef "39051" "Vampire" MojoSet.Horror 1)
  { cdTraits = fromList [MojoTrait.Vampire]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39052 :: CardDef
mojo39052 = (mojoCardDef "39052" "Werewolf Pack" MojoSet.Horror 1)
  { cdTraits = fromList [MojoTrait.Werewolf]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39054 :: CardDef
mojo39054 = (mojoCardDef "39054" "Avalanche 9.0" MojoSet.SciFi 1)
  { cdTraits = fromList [MojoTrait.Program]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Star]
  }

mojo39055 :: CardDef
mojo39055 = (mojoCardDef "39055" "Blob 3.14" MojoSet.SciFi 1)
  { cdTraits = fromList [MojoTrait.Program]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Star]
  }

mojo39056 :: CardDef
mojo39056 = (mojoCardDef "39056" "Magneto 2.6" MojoSet.SciFi 1)
  { cdTraits = fromList [MojoTrait.Elite, MojoTrait.Program]
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Villainous]
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39057 :: CardDef
mojo39057 = (mojoCardDef "39057" "Pyro 4.0" MojoSet.SciFi 1)
  { cdTraits = fromList [MojoTrait.Program]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Star]
  }

mojo39058 :: CardDef
mojo39058 = (mojoCardDef "39058" "Toad 2.0" MojoSet.SciFi 1)
  { cdTraits = fromList [MojoTrait.Program]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39068 :: CardDef
mojo39068 = (mojoCardDef "39068" "Card Shark" MojoSet.Western 1)
  { cdTraits = fromList [MojoTrait.Creature, MojoTrait.Elite]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39069 :: CardDef
mojo39069 = (mojoCardDef "39069" "Gunslinger" MojoSet.Western 2)
  { cdTraits = fromList [MojoTrait.Outlaw]
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Quickstrike]
  , cdBoostIcons = [MojoBoost.Boost]
  }

-- NeXt Evolution: Cable

cable40032 :: CardDef
cable40032 = (minion "40032" "Stryfe" [NE.MutantLiberationFront, NE.Psionic] [NE.Boost, NE.Boost, NE.Boost] CableNemesis 1) { cdUnique = True, cdDeckLimit = Just 1, cdKeywords = fromList [NE.Villainous] }

-- NeXt Evolution: Domino
topaz :: CardDef
topaz = CardDef
  { cdCardCode = "40066", cdName = "Topaz", cdCost = Nothing
  , cdTraits = fromList [NET.Mutant], cdKeywords = fromList []
  , cdCardType = MinionType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [], cdBoostIcons = [NEB.Boost, NEB.Boost, NEB.Boost]
  , cdEncounterSet = Just NES.DominoNemesis, cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
prototype :: CardDef
prototype = CardDef
  { cdCardCode = "40068", cdName = "Prototype", cdCost = Nothing
  , cdTraits = fromList [NET.Mutant], cdKeywords = fromList []
  , cdCardType = MinionType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [], cdBoostIcons = [NEB.Boost, NEB.Boost]
  , cdEncounterSet = Just NES.DominoNemesis, cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution campaign and scenarios

ne40094 :: CardDef
ne40094 = CardDef
  { cdCardCode = "40094"
  , cdName = "Arclight"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Marauder]
  , cdKeywords = fromList [NEKeyword.Retaliate (1)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantSlayers
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

ne40095 :: CardDef
ne40095 = CardDef
  { cdCardCode = "40095"
  , cdName = "Blockbuster"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Marauder]
  , cdKeywords = fromList [NEKeyword.Guard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantSlayers
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

ne40096 :: CardDef
ne40096 = CardDef
  { cdCardCode = "40096"
  , cdName = "Chimera"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder, NETrait.Psionic]
  , cdKeywords = fromList [NEKeyword.Patrol]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantSlayers
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

ne40097 :: CardDef
ne40097 = CardDef
  { cdCardCode = "40097"
  , cdName = "Greycrow"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder, NETrait.Mercenary]
  , cdKeywords = fromList [NEKeyword.Retaliate (1)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantSlayers
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

ne40098 :: CardDef
ne40098 = CardDef
  { cdCardCode = "40098"
  , cdName = "Harpoon"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Brute, NETrait.Marauder]
  , cdKeywords = fromList [NEKeyword.Guard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantSlayers
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

ne40099 :: CardDef
ne40099 = CardDef
  { cdCardCode = "40099"
  , cdName = "Riptide"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Assassin, NETrait.Marauder]
  , cdKeywords = fromList [NEKeyword.Patrol]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantSlayers
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

ne40100 :: CardDef
ne40100 = CardDef
  { cdCardCode = "40100"
  , cdName = "Vertigo"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder, NETrait.Mutate]
  , cdKeywords = fromList [NEKeyword.Guard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantSlayers
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

ne40112 :: CardDef
ne40112 = CardDef
  { cdCardCode = "40112"
  , cdName = "Gorgeous George"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.NastyBoy]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.NastyBoy]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NastyBoys
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

ne40113 :: CardDef
ne40113 = CardDef
  { cdCardCode = "40113"
  , cdName = "Hairbag"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.NastyBoy]
  , cdKeywords = fromList [NEKeyword.Surge, NEKeyword.Teamwork NETrait.NastyBoy]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NastyBoys
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

ne40114 :: CardDef
ne40114 = CardDef
  { cdCardCode = "40114"
  , cdName = "Ramrod"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.NastyBoy]
  , cdKeywords = fromList [NEKeyword.Retaliate (1), NEKeyword.Teamwork NETrait.NastyBoy]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NastyBoys
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

ne40115 :: CardDef
ne40115 = CardDef
  { cdCardCode = "40115"
  , cdName = "Ruckus"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.NastyBoy]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.NastyBoy]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NastyBoys
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

ne40116 :: CardDef
ne40116 = CardDef
  { cdCardCode = "40116"
  , cdName = "Slab"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.NastyBoy]
  , cdKeywords = fromList [NEKeyword.Toughness, NEKeyword.Teamwork NETrait.NastyBoy]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.NastyBoys
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

ne40132 :: CardDef
ne40132 = CardDef
  { cdCardCode = "40132"
  , cdName = "Black Tom Cassidy"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Criminal, NETrait.Elite]
  , cdKeywords = fromList [NEKeyword.Villainous]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.BlackTomCassidy
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

ne40133 :: CardDef
ne40133 = CardDef
  { cdCardCode = "40133"
  , cdName = "Creeping Willow"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Wood]
  , cdKeywords = fromList [NEKeyword.Guard, NEKeyword.Quickstrike]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.BlackTomCassidy
  , cdEncounterSetQuantity = Just 4
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

ne40145 :: CardDef
ne40145 = CardDef
  { cdCardCode = "40145"
  , cdName = "Sinister Soldier"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Clone]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.MisterSinister
  , cdEncounterSetQuantity = Just 2
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

ne40174 :: CardDef
ne40174 = CardDef
  { cdCardCode = "40174"
  , cdName = "Zero"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront]
  , cdKeywords = fromList [NEKeyword.Guard, NEKeyword.Patrol, NEKeyword.Toughness]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
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

ne40180 :: CardDef
ne40180 = CardDef
  { cdCardCode = "40180"
  , cdName = "Strobe"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront]
  , cdKeywords = fromList [NEKeyword.Patrol]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.ExtremeMeasures
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

ne40181 :: CardDef
ne40181 = CardDef
  { cdCardCode = "40181"
  , cdName = "Tempo"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.ExtremeMeasures
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

ne40182 :: CardDef
ne40182 = CardDef
  { cdCardCode = "40182"
  , cdName = "Thumbelina"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.ExtremeMeasures
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

ne40183 :: CardDef
ne40183 = CardDef
  { cdCardCode = "40183"
  , cdName = "Wildside"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.ExtremeMeasures
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

ne40185 :: CardDef
ne40185 = CardDef
  { cdCardCode = "40185"
  , cdName = "Dragoness"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantInsurrection
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

ne40186 :: CardDef
ne40186 = CardDef
  { cdCardCode = "40186"
  , cdName = "Forearm"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantInsurrection
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

ne40187 :: CardDef
ne40187 = CardDef
  { cdCardCode = "40187"
  , cdName = "Reaper"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantInsurrection
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

ne40188 :: CardDef
ne40188 = CardDef
  { cdCardCode = "40188"
  , cdName = "Samurai"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.MutantLiberationFront]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MutantInsurrection
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

ne40198 :: CardDef
ne40198 = CardDef
  { cdCardCode = "40198"
  , cdName = "Lady Mastermind"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder]
  , cdKeywords = fromList [NEKeyword.Surge]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
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

ne40199 :: CardDef
ne40199 = CardDef
  { cdCardCode = "40199"
  , cdName = "Malice"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder]
  , cdKeywords = fromList [NEKeyword.Surge]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
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

ne40200 :: CardDef
ne40200 = CardDef
  { cdCardCode = "40200"
  , cdName = "Scrambler"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder]
  , cdKeywords = fromList [NEKeyword.Surge]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
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

ne40201 :: CardDef
ne40201 = CardDef
  { cdCardCode = "40201"
  , cdName = "Vanisher"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Marauder]
  , cdKeywords = fromList [NEKeyword.Surge]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
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

hood24015 :: CardDef
hood24015 = (hoodCardDef "24015" "Griffin" HoodSet.BeastyBoys MinionType 1)
  { cdTraits = fromList [HoodTrait.Brute, HoodTrait.MastersOfEvil]
  , cdKeywords = fromList [HoodKeyword.Quickstrike]
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24016 :: CardDef
hood24016 = (hoodCardDef "24016" "Mandrill" HoodSet.BeastyBoys MinionType 1)
  { cdTraits = fromList [HoodTrait.Brute, HoodTrait.CrossfiresCrew]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24018 :: CardDef
hood24018 = (hoodCardDef "24018" "Brothers Grimm" HoodSet.BrothersGrimm MinionType 1)
  { cdTraits = fromList [HoodTrait.MastersOfEvil, HoodTrait.Mystic]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Star]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24024 :: CardDef
hood24024 = (hoodCardDef "24024" "Controller" HoodSet.CrossfiresCrew MinionType 1)
  { cdTraits = fromList [HoodTrait.Brute, HoodTrait.CrossfiresCrew]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24025 :: CardDef
hood24025 = (hoodCardDef "24025" "Corruptor" HoodSet.CrossfiresCrew MinionType 1)
  { cdTraits = fromList [HoodTrait.Criminal, HoodTrait.CrossfiresCrew]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Star]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24026 :: CardDef
hood24026 = (hoodCardDef "24026" "Crossfire" HoodSet.CrossfiresCrew MinionType 1)
  { cdTraits = fromList [HoodTrait.CrossfiresCrew, HoodTrait.MastersOfEvil]
  , cdKeywords = fromList [HoodKeyword.Quickstrike]
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24027 :: CardDef
hood24027 = (hoodCardDef "24027" "Mister Fear" HoodSet.CrossfiresCrew MinionType 1)
  { cdTraits = fromList [HoodTrait.Criminal, HoodTrait.CrossfiresCrew]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Star]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24034 :: CardDef
hood24034 = (hoodCardDef "24034" "Calvin Zabo" HoodSet.MisterHydeSet MinionType 1)
  { cdTraits = fromList [HoodTrait.Elite, HoodTrait.MastersOfEvil]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24035 :: CardDef
hood24035 = (hoodCardDef "24035" "Mister Hyde" HoodSet.MisterHydeSet MinionType 1)
  { cdTraits = fromList [HoodTrait.Brute, HoodTrait.Elite, HoodTrait.MastersOfEvil]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24041 :: CardDef
hood24041 = (hoodCardDef "24041" "Armored Guard" HoodSet.RansackedArmory MinionType 2)
  { cdTraits = fromList [HoodTrait.Mercenary]
  , cdKeywords = fromList [HoodKeyword.Guard, HoodKeyword.Toughness]
  , cdBoostIcons = [HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24043 :: CardDef
hood24043 = (hoodCardDef "24043" "Beetle" HoodSet.SinisterSyndicate MinionType 1)
  { cdTraits = fromList [HoodTrait.Criminal]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Star]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24044 :: CardDef
hood24044 = (hoodCardDef "24044" "Boomerang" HoodSet.SinisterSyndicate MinionType 1)
  { cdTraits = fromList [HoodTrait.Criminal, HoodTrait.MastersOfEvil]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Star]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24045 :: CardDef
hood24045 = (hoodCardDef "24045" "Shocker" HoodSet.SinisterSyndicate MinionType 1)
  { cdTraits = fromList [HoodTrait.Criminal, HoodTrait.MastersOfEvil]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Star]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24046 :: CardDef
hood24046 = (hoodCardDef "24046" "Speed Demon" HoodSet.SinisterSyndicate MinionType 1)
  { cdTraits = fromList [HoodTrait.Criminal]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Star]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24047 :: CardDef
hood24047 = (hoodCardDef "24047" "White Rabbit" HoodSet.SinisterSyndicate MinionType 1)
  { cdTraits = fromList [HoodTrait.Criminal]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Star]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24065 :: CardDef
hood24065 = (hoodCardDef "24065" "Wrecker" HoodSet.WreckingCrewModular MinionType 1)
  { cdTraits = fromList [HoodTrait.Brute, HoodTrait.Elite]
  , cdKeywords = fromList [HoodKeyword.Villainous]
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24066 :: CardDef
hood24066 = (hoodCardDef "24066" "Bulldozer" HoodSet.WreckingCrewModular MinionType 1)
  { cdTraits = fromList [HoodTrait.Brute, HoodTrait.Elite]
  , cdKeywords = fromList [HoodKeyword.Villainous]
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24067 :: CardDef
hood24067 = (hoodCardDef "24067" "Piledriver" HoodSet.WreckingCrewModular MinionType 1)
  { cdTraits = fromList [HoodTrait.Brute, HoodTrait.Elite]
  , cdKeywords = fromList [HoodKeyword.Villainous, HoodKeyword.Retaliate 1]
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24068 :: CardDef
hood24068 = (hoodCardDef "24068" "Thunderball" HoodSet.WreckingCrewModular MinionType 1)
  { cdTraits = fromList [HoodTrait.Brute, HoodTrait.Elite]
  , cdKeywords = fromList [HoodKeyword.Villainous]
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = True
  , cdAcceleration = 0
  , cdHazards = 0
  }

-- Psylocke

chimera :: CardDef
chimera = CardDef
  { cdCardCode = "41026", cdName = "Chimera", cdCost = Nothing
  , cdTraits = fromList [PsyTrait.Marauder, PsyTrait.Psionic], cdKeywords = fromList []
  , cdCardType = PsyCard.MinionType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [], cdBoostIcons = [PsyBoost.Boost, PsyBoost.Boost, PsyBoost.Boost]
  , cdEncounterSet = Just PsySet.PsylockeNemesis, cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

-- Angel

harpoon :: CardDef
harpoon = CardDef
  { cdCardCode = "42025"
  , cdName = "Harpoon"
  , cdCost = Nothing
  , cdTraits = fromList [AnTrait.Brute, AnTrait.Marauder]
  , cdKeywords = fromList []
  , cdCardType = AnCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AnBoost.Boost, AnBoost.Boost]
  , cdEncounterSet = Just AnSet.AngelNemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

-- X-23

ladyDeathstrikeX23 :: CardDef
ladyDeathstrikeX23 = CardDef
  { cdCardCode = "43029"
  , cdName = "Lady Deathstrike"
  , cdCost = Nothing
  , cdTraits = fromList [AnTrait.Elite, AnTrait.Reaver]
  , cdKeywords = fromList []
  , cdCardType = AnCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AnBoost.Boost, AnBoost.Boost, AnBoost.Boost]
  , cdEncounterSet = Just AnSet.X23Nemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

-- Deadpool

butler :: CardDef
butler = DpCard.CardDef
  { cdCardCode = "44033"
  , cdName = DpName.Name "Butler" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [DpTrait.Scientist]
  , cdKeywords = fromList []
  , cdCardType = DpCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [DpBoost.Star]
  , cdEncounterSet = Just DpSet.DeadpoolNemesis
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
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

mutatedSoldier :: CardDef
mutatedSoldier = DpCard.CardDef
  { cdCardCode = "44036"
  , cdName = DpName.Name "Mutated Soldier" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [DpTrait.Mutate, DpTrait.Soldier]
  , cdKeywords = fromList [DpKeyword.Toughness]
  , cdCardType = DpCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [DpBoost.Boost, DpBoost.Boost]
  , cdEncounterSet = Just DpSet.DeadpoolNemesis
  , cdEncounterSetQuantity = Just 2
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
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

dreadpool :: CardDef
dreadpool = DpCard.CardDef
  { cdCardCode = "44038"
  , cdName = DpName.Name "Dreadpool" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [DpTrait.Assassin, DpTrait.Elite]
  , cdKeywords = fromList []
  , cdCardType = DpCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [DpBoost.Boost, DpBoost.Boost, DpBoost.Boost]
  , cdEncounterSet = Just DpSet.DreadpoolSet
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
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Bishop

trevorFitzroy :: CardDef
trevorFitzroy = CardDef
  { cdCardCode = "45026"
  , cdName = "Trevor Fitzroy"
  , cdCost = Nothing
  , cdTraits = fromList [BpTrait.Elite, BpTrait.Temporal]
  , cdKeywords = fromList [BpKeyword.Quickstrike]
  , cdCardType = BpCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = BpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [BpBoost.Boost, BpBoost.Boost, BpBoost.Boost]
  , cdEncounterSet = Just BpSet.BishopNemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = BpCost.NoCost
  }

bantam :: CardDef
bantam = CardDef
  { cdCardCode = "45028"
  , cdName = "Bantam"
  , cdCost = Nothing
  , cdTraits = fromList [BpTrait.Temporal]
  , cdKeywords = fromList []
  , cdCardType = BpCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = BpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [BpBoost.Boost, BpBoost.Boost]
  , cdEncounterSet = Just BpSet.BishopNemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = BpCost.NoCost
  }

-- Age of Apocalypse: Magik
belasco :: CardDef
belasco = CardDef
  { cdCardCode = "45054"
  , cdName = "Belasco"
  , cdCost = Nothing
  , cdTraits = fromList [AoaMT.Elite, AoaMT.Limbo]
  , cdKeywords = fromList [AoaMK.Villainous]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [AoaMB.Boost, AoaMB.Boost, AoaMB.Boost]
  , cdEncounterSet = Just AoaMS.MagikNemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse: Magik
sym :: CardDef
sym = CardDef
  { cdCardCode = "45056"
  , cdName = "S'ym"
  , cdCost = Nothing
  , cdTraits = fromList [AoaMT.Limbo]
  , cdKeywords = fromList [AoaMK.Guard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [AoaMB.Boost, AoaMB.Boost]
  , cdEncounterSet = Just AoaMS.MagikNemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse: Magik
witchfire :: CardDef
witchfire = CardDef
  { cdCardCode = "45057"
  , cdName = "Witchfire"
  , cdCost = Nothing
  , cdTraits = fromList [AoaMT.Limbo]
  , cdKeywords = fromList [AoaMK.Quickstrike]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [AoaMB.Boost, AoaMB.Boost]
  , cdEncounterSet = Just AoaMS.MagikNemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Age of Apocalypse campaign and scenarios

infiniteHunter :: CardDef
infiniteHunter = CardDef
  { cdCardCode = "45065"
  , cdName = "Infinite Hunter"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Infinite]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaUnus
  , cdEncounterSetQuantity = Just 2
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

infiniteSoldier :: CardDef
infiniteSoldier = CardDef
  { cdCardCode = "45069"
  , cdName = "Infinite Soldier"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Infinite]
  , cdKeywords = fromList [NEKeyword.Guard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaInfinites
  , cdEncounterSetQuantity = Just 5
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

ahab :: CardDef
ahab = CardDef
  { cdCardCode = "45097"
  , cdName = "Ahab"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Cyborg, NETrait.Tracker]
  , cdKeywords = fromList [NEKeyword.Toughness]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaHounds
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

hound :: CardDef
hound = CardDef
  { cdCardCode = "45098"
  , cdName = "Hound"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Tracker]
  , cdKeywords = fromList [NEKeyword.Guard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaHounds
  , cdEncounterSetQuantity = Just 4
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

gauntlet :: CardDef
gauntlet = CardDef
  { cdCardCode = "45112"
  , cdName = "Gauntlet"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.DarkRiders]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.DarkRiders]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaDarkRiders
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

barrage :: CardDef
barrage = CardDef
  { cdCardCode = "45113"
  , cdName = "Barrage"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.DarkRiders]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.DarkRiders]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaDarkRiders
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

hardDrive :: CardDef
hardDrive = CardDef
  { cdCardCode = "45114"
  , cdName = "Hard-Drive"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.DarkRiders]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.DarkRiders]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaDarkRiders
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

tusk :: CardDef
tusk = CardDef
  { cdCardCode = "45115"
  , cdName = "Tusk"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.DarkRiders]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.DarkRiders]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaDarkRiders
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

psynapse :: CardDef
psynapse = CardDef
  { cdCardCode = "45116"
  , cdName = "Psynapse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.DarkRiders]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.DarkRiders]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaDarkRiders
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

pterosaur :: CardDef
pterosaur = CardDef
  { cdCardCode = "45128"
  , cdName = "Pterosaur"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Aerial, NETrait.Creature]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaSavageLand
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

velociraptor :: CardDef
velociraptor = CardDef
  { cdCardCode = "45129"
  , cdName = "Velociraptor"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Creature]
  , cdKeywords = fromList [NEKeyword.Quickstrike]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaSavageLand
  , cdEncounterSetQuantity = Just 2
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

giantApe :: CardDef
giantApe = CardDef
  { cdCardCode = "45130"
  , cdName = "Giant Ape"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Creature]
  , cdKeywords = fromList [NEKeyword.Guard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaSavageLand
  , cdEncounterSetQuantity = Just 2
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

magistrate :: CardDef
magistrate = CardDef
  { cdCardCode = "45134"
  , cdName = "Magistrate"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Genosha]
  , cdKeywords = fromList [NEKeyword.Patrol]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaGenosha
  , cdEncounterSetQuantity = Just 2
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

armoredUnibike :: CardDef
armoredUnibike = CardDef
  { cdCardCode = "45135"
  , cdName = "Armored Unibike"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Genosha, NETrait.Vehicle]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaGenosha
  , cdEncounterSetQuantity = Just 2
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

genoshanMech :: CardDef
genoshanMech = CardDef
  { cdCardCode = "45136"
  , cdName = "Genoshan Mech"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Genosha, NETrait.Vehicle]
  , cdKeywords = fromList [NEKeyword.Guard, NEKeyword.Toughness]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaGenosha
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

gladiator :: CardDef
gladiator = CardDef
  { cdCardCode = "45140"
  , cdName = "Gladiator"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Elite, NETrait.ImperialGuard]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.ImperialGuard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaBlueMoon
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

oracle :: CardDef
oracle = CardDef
  { cdCardCode = "45141"
  , cdName = "Oracle"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.ImperialGuard]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.ImperialGuard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaBlueMoon
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

manta :: CardDef
manta = CardDef
  { cdCardCode = "45142"
  , cdName = "Manta"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.ImperialGuard]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.ImperialGuard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaBlueMoon
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

earthquake :: CardDef
earthquake = CardDef
  { cdCardCode = "45143"
  , cdName = "Earthquake"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.ImperialGuard]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.ImperialGuard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaBlueMoon
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

warstar :: CardDef
warstar = CardDef
  { cdCardCode = "45144"
  , cdName = "Warstar"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.ImperialGuard]
  , cdKeywords = fromList [NEKeyword.Teamwork NETrait.ImperialGuard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaBlueMoon
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

ozymandias :: CardDef
ozymandias = CardDef
  { cdCardCode = "45159"
  , cdName = "Ozymandias"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.ClanAkkaba, NETrait.Elite]
  , cdKeywords = fromList [NEKeyword.Toughness, NEKeyword.Villainous]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaClanAkkaba
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

scarab :: CardDef
scarab = CardDef
  { cdCardCode = "45160"
  , cdName = "Scarab"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.ClanAkkaba]
  , cdKeywords = fromList [NEKeyword.Quickstrike]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaClanAkkaba
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

clanAkkabaZealot :: CardDef
clanAkkabaZealot = CardDef
  { cdCardCode = "45161"
  , cdName = "Clan Akkaba Zealot"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.ClanAkkaba]
  , cdKeywords = fromList [NEKeyword.Guard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaClanAkkaba
  , cdEncounterSetQuantity = Just 3
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

agentOfApocalypse :: CardDef
agentOfApocalypse = CardDef
  { cdCardCode = "45164"
  , cdName = "Agent of Apocalypse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.ClanAkkaba]
  , cdKeywords = fromList [NEKeyword.Guard]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AgeOfApocalypse
  , cdEncounterSetQuantity = Just 2
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

misterSinisterMinion :: CardDef
misterSinisterMinion = CardDef
  { cdCardCode = "45179a"
  , cdName = "Mister Sinister"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Elite, NETrait.Overseer]
  , cdKeywords = fromList [NEKeyword.Victory (5)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaOverseer
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

misterSinisterOverseer :: CardDef
misterSinisterOverseer = CardDef
  { cdCardCode = "45179b"
  , cdName = "Mister Sinister"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Elite, NETrait.Prelate]
  , cdKeywords = fromList [NEKeyword.Toughness, NEKeyword.Villainous, NEKeyword.Retaliate (1), NEKeyword.Victory (3)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaPrelates
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

theShadowKingMinion :: CardDef
theShadowKingMinion = CardDef
  { cdCardCode = "45180a"
  , cdName = "The Shadow King"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Elite, NETrait.Overseer]
  , cdKeywords = fromList [NEKeyword.Victory (5)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaOverseer
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

theShadowKingOverseer :: CardDef
theShadowKingOverseer = CardDef
  { cdCardCode = "45180b"
  , cdName = "The Shadow King"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Elite, NETrait.Prelate]
  , cdKeywords = fromList [NEKeyword.Toughness, NEKeyword.Victory (3)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaPrelates
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

abyssMinion :: CardDef
abyssMinion = CardDef
  { cdCardCode = "45181a"
  , cdName = "Abyss"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Elite, NETrait.Overseer]
  , cdKeywords = fromList [NEKeyword.Victory (5)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaOverseer
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

abyssOverseer :: CardDef
abyssOverseer = CardDef
  { cdCardCode = "45181b"
  , cdName = "Abyss"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Elite, NETrait.Prelate]
  , cdKeywords = fromList [NEKeyword.Toughness, NEKeyword.Victory (3)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaPrelates
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

sugarManMinion :: CardDef
sugarManMinion = CardDef
  { cdCardCode = "45182a"
  , cdName = "Sugar Man"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Elite, NETrait.Overseer]
  , cdKeywords = fromList [NEKeyword.Victory (5)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaOverseer
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

sugarManOverseer :: CardDef
sugarManOverseer = CardDef
  { cdCardCode = "45182b"
  , cdName = "Sugar Man"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Elite, NETrait.Prelate]
  , cdKeywords = fromList [NEKeyword.Toughness, NEKeyword.Victory (3)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaPrelates
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

mikhailRasputinMinion :: CardDef
mikhailRasputinMinion = CardDef
  { cdCardCode = "45183a"
  , cdName = "Mikhail Rasputin"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Elite, NETrait.Overseer]
  , cdKeywords = fromList [NEKeyword.Victory (5)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.AoaOverseer
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

mikhailRasputinOverseer :: CardDef
mikhailRasputinOverseer = CardDef
  { cdCardCode = "45183b"
  , cdName = "Mikhail Rasputin"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Elite, NETrait.Prelate]
  , cdKeywords = fromList [NEKeyword.Toughness, NEKeyword.Victory (3)]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaPrelates
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

-- Magneto

exodusMagneto :: CardDef
exodusMagneto = CardDef
  { cdCardCode = "49028"
  , cdName = "Exodus"
  , cdCost = Nothing
  , cdTraits = fromList [MgTrait.Acolyte, MgTrait.Elite, MgTrait.Psionic]
  , cdKeywords = fromList [MgKeyword.Toughness, MgKeyword.Villainous, MgKeyword.Steady]
  , cdCardType = MgCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [MgBoost.Boost, MgBoost.Boost, MgBoost.Boost]
  , cdEncounterSet = Just MgSet.MagnetoNemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

fabianCortez :: CardDef
fabianCortez = CardDef
  { cdCardCode = "49030"
  , cdName = "Fabian Cortez"
  , cdCost = Nothing
  , cdTraits = fromList [MgTrait.Acolyte]
  , cdKeywords = fromList [MgKeyword.Guard]
  , cdCardType = MgCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [MgBoost.Star]
  , cdEncounterSet = Just MgSet.MagnetoNemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

frenzy :: CardDef
frenzy = CardDef
  { cdCardCode = "49031"
  , cdName = "Frenzy"
  , cdCost = Nothing
  , cdTraits = fromList [MgTrait.Acolyte]
  , cdKeywords = fromList [MgKeyword.Quickstrike]
  , cdCardType = MgCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [MgBoost.Star]
  , cdEncounterSet = Just MgSet.MagnetoNemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

sebastianShaw :: CardDef
sebastianShaw = CardDef
  { cdCardCode = "49038"
  , cdName = "Sebastian Shaw"
  , cdCost = Nothing
  , cdTraits = fromList [MgTrait.Hellfire]
  , cdKeywords = fromList [MgKeyword.Toughness, MgKeyword.Villainous]
  , cdCardType = MgCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [MgBoost.Boost, MgBoost.Boost, MgBoost.Boost]
  , cdEncounterSet = Just MgSet.HellfireSet
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

selene :: CardDef
selene = CardDef
  { cdCardCode = "49039"
  , cdName = "Selene"
  , cdCost = Nothing
  , cdTraits = fromList [MgTrait.Hellfire]
  , cdKeywords = fromList [MgKeyword.Villainous, MgKeyword.Quickstrike]
  , cdCardType = MgCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [MgBoost.Star]
  , cdEncounterSet = Just MgSet.HellfireSet
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

hellfirePawnMagneto :: CardDef
hellfirePawnMagneto = CardDef
  { cdCardCode = "49040"
  , cdName = "Hellfire Pawn"
  , cdCost = Nothing
  , cdTraits = fromList [MgTrait.Hellfire]
  , cdKeywords = fromList [MgKeyword.Guard, MgKeyword.Patrol, MgKeyword.Surge]
  , cdCardType = MgCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [MgBoost.Star]
  , cdEncounterSet = Just MgSet.HellfireSet
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

-- Iceman
pyroIceman :: CardDef
pyroIceman = IcemanPack.pyroIceman

-- Iceman
sauron :: CardDef
sauron = IcemanPack.sauron

-- Nightcrawler
nc48027 :: CardDef
nc48027 = CardDef
  { cdCardCode = "48027"
  , cdName = "Azazel"
  , cdCost = Nothing
  , cdTraits = fromList [NcTrait.Elite, NcTrait.Neyaphem]
  , cdKeywords = fromList [NcKeyword.Quickstrike]
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [NcBoost.Star]
  , cdEncounterSet = Just NcSet.NightcrawlerNemesis
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
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48034 :: CardDef
nc48034 = CardDef
  { cdCardCode = "48034"
  , cdName = "Queen of Hearts"
  , cdCost = Nothing
  , cdTraits = fromList [NcTrait.Criminal, NcTrait.Queen]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [NcBoost.Boost, NcBoost.Boost, NcBoost.Boost]
  , cdEncounterSet = Just NcSet.CrazyGangSet
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
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48035 :: CardDef
nc48035 = CardDef
  { cdCardCode = "48035"
  , cdName = "Jester"
  , cdCost = Nothing
  , cdTraits = fromList [NcTrait.Criminal]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [NcBoost.Boost]
  , cdEncounterSet = Just NcSet.CrazyGangSet
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
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48036 :: CardDef
nc48036 = CardDef
  { cdCardCode = "48036"
  , cdName = "Executioner"
  , cdCost = Nothing
  , cdTraits = fromList [NcTrait.Criminal]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [NcBoost.Boost, NcBoost.Boost]
  , cdEncounterSet = Just NcSet.CrazyGangSet
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
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Nightcrawler
nc48037 :: CardDef
nc48037 = CardDef
  { cdCardCode = "48037"
  , cdName = "Tweedledope"
  , cdCost = Nothing
  , cdTraits = fromList [NcTrait.Brute, NcTrait.Criminal]
  , cdKeywords = fromList []
  , cdCardType = MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [NcBoost.Boost]
  , cdEncounterSet = Just NcSet.CrazyGangSet
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
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Agents of Shield players

aosP50030 :: CardDef
aosP50030 = CardDef
  { cdCardCode = "50030"
  , cdName = "Controller"
  , cdCost = Nothing
  , cdTraits = fromList [AospTrait.Elite, AospTrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = AospCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AospBoost.Boost, AospBoost.Boost, AospBoost.Boost]
  , cdEncounterSet = Just AospSet.MariaHillNemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }

aosP50060 :: CardDef
aosP50060 = CardDef
  { cdCardCode = "50060"
  , cdName = "Orion"
  , cdCost = Nothing
  , cdTraits = fromList [AospTrait.Leviathan]
  , cdKeywords = fromList [AospKeyword.Toughness]
  , cdCardType = AospCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AospBoost.Boost, AospBoost.Boost, AospBoost.Boost]
  , cdEncounterSet = Just AospSet.NickFuryNemesis
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }

aosP50062 :: CardDef
aosP50062 = CardDef
  { cdCardCode = "50062"
  , cdName = "Leviathan Soldier"
  , cdCost = Nothing
  , cdTraits = fromList [AospTrait.Leviathan]
  , cdKeywords = fromList [AospKeyword.Toughness]
  , cdCardType = AospCard.MinionType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AospBoost.Boost]
  , cdEncounterSet = Just AospSet.NickFuryNemesis
  , cdEncounterSetQuantity = Just 2
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
  , cdAbilityCost = AospCost.NoCost
  }

aos50072 :: CardDef
aos50072 = AosEncounter.aos50072

aos50073 :: CardDef
aos50073 = AosEncounter.aos50073

aos50080 :: CardDef
aos50080 = AosEncounter.aos50080

aos50083 :: CardDef
aos50083 = AosEncounter.aos50083

aos50084 :: CardDef
aos50084 = AosEncounter.aos50084

aos50093 :: CardDef
aos50093 = AosEncounter.aos50093

aos50094 :: CardDef
aos50094 = AosEncounter.aos50094

aos50098 :: CardDef
aos50098 = AosEncounter.aos50098

aos50099 :: CardDef
aos50099 = AosEncounter.aos50099

aos50100 :: CardDef
aos50100 = AosEncounter.aos50100

aos50113 :: CardDef
aos50113 = AosEncounter.aos50113

aos50120 :: CardDef
aos50120 = AosEncounter.aos50120

aos50125 :: CardDef
aos50125 = AosEncounter.aos50125

aos50126 :: CardDef
aos50126 = AosEncounter.aos50126

aos50133 :: CardDef
aos50133 = AosEncounter.aos50133

aos50139 :: CardDef
aos50139 = AosEncounter.aos50139

aos50143 :: CardDef
aos50143 = AosEncounter.aos50143

aos50148 :: CardDef
aos50148 = AosEncounter.aos50148

aos50152 :: CardDef
aos50152 = AosEncounter.aos50152

aos50156 :: CardDef
aos50156 = AosEncounter.aos50156

aos50161 :: CardDef
aos50161 = AosEncounter.aos50161

aos50172 :: CardDef
aos50172 = AosEncounter.aos50172

aos50178 :: CardDef
aos50178 = AosEncounter.aos50178
