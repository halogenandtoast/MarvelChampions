module Marvel.Attachment.Cards where

import Marvel.FearNoEvil.Cards qualified as OwlDefs

import Marvel.Shadowland.Cards qualified as WhisperDefs

import Marvel.Shadowland.Cards qualified as SpellsDefs

import Marvel.Shadowland.Cards qualified as DarkDefs

import Marvel.Shadowland.Cards qualified as StreetTrapsCards

import Marvel.CampaignPlayers.EncounterCards qualified as CN
import Marvel.JessicaJones.Cards qualified as JL
import Marvel.AgentsOfShield.EncounterCards qualified as AosEncounter
import Marvel.FalconWinter.Cards qualified as FalconWinter

import Marvel.BlackPantherShuri.Cards qualified as BlackPantherShuriPack

import Marvel.TricksterTakeover.Cards qualified as TT

import Marvel.Window.Types qualified as NcWindow ()

import Marvel.Trait.Types qualified as NcTrait

import Marvel.Resource.Types qualified as NcResource ()

import Marvel.Name qualified as NcName ()

import Marvel.Matchers qualified as NcMatch ()

import Marvel.Keyword qualified as NcKeyword ()

import Marvel.EncounterSet qualified as NcSet

import Marvel.Criteria qualified as NcCriteria

import Marvel.Cost.Types qualified as NcCost

import Marvel.Card.Def qualified as NcCard ()

import Marvel.Boost qualified as NcBoost

import Marvel.Aspect qualified as NcAspect ()

import Marvel.Ability.Types qualified as NcAbility ()

import Marvel.Iceman.Cards qualified as IcemanPack
import Marvel.Jubilee.Cards qualified as Jubilee




import Marvel.Ability.Types qualified as DpAbility ()
import Marvel.Aspect qualified as DpAspect ()
import Marvel.Boost qualified as DpBoost
import Marvel.Card.Def qualified as DpCard
import Marvel.Cost.Types qualified as DpCost
import Marvel.Criteria qualified as DpCriteria
import Marvel.EncounterSet qualified as DpSet
import Marvel.Keyword qualified as DpKeyword ()
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
import Marvel.Keyword qualified as HoodKeyword ()
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
import Marvel.Boost qualified as NE
import Marvel.Ability.Types qualified as NEAbility ()

import Marvel.Trait qualified as MojoTrait
import Marvel.EncounterSet qualified as MojoSet
import Marvel.Keyword qualified as MojoKeyword
import Marvel.GameValue.Types qualified as MojoValue ()
import Marvel.Boost qualified as MojoBoost

import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.Trait qualified as MGTrait
import Marvel.Resource qualified as MGResource ()
import Marvel.Boost qualified as MGBoost
import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait
import Marvel.Keyword qualified as SMKeyword
import Marvel.GameValue.Types qualified as SMValue ()
import Marvel.Boost qualified as SMBoost

import Marvel.Cost.Types
import Marvel.Window ()
import Marvel.Matchers ()
import Marvel.GameValue.Types qualified as MGValue ()

import Marvel.Hercules.Cards qualified as HC

import Marvel.Prelude
import Marvel.CivilWar.Cards qualified as CWDefs


import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Data.HashSet (singleton)
import Marvel.Keyword
import Marvel.Name
import Marvel.Trait hiding (Temporal)
import Marvel.Trait qualified as Trait

allAttachments :: HashMap CardCode CardDef
allAttachments =
  fromList $ [(toCardCode def,def) | def <- [CWDefs.cw56084,CWDefs.cw56080,CWDefs.cw56098,CWDefs.cw56143,CWDefs.cw56174,CWDefs.cw57007,CWDefs.cw57046a,CWDefs.cw57046b,CWDefs.cw56065,CWDefs.cw56066,CWDefs.cw56067,CWDefs.cw56068,CWDefs.cw56069,CWDefs.cw56099,CWDefs.cw56144,CWDefs.cw56145,CWDefs.cw57008,CWDefs.cw57047,CWDefs.cw57048,CWDefs.cw57050,CWDefs.cw56100,CWDefs.cw56175,CWDefs.cw57049]] <>
    map
      (toFst toCardCode)
      [ CN.enc56026, CN.enc56057, CN.enc60035
  , theAccused
  , HC.hc59002, HC.hc59003, HC.hc59039
  , JL.jl62031
  , StreetTrapsCards.street65046, DarkDefs.street65018, DarkDefs.street65019, SpellsDefs.street65062, SpellsDefs.street65063, SpellsDefs.street65064, SpellsDefs.street65065, WhisperDefs.street65040, OwlDefs.street60186, OwlDefs.street60187, OwlDefs.street60188, OwlDefs.street60191, OwlDefs.street60195, OwlDefs.street60196, TT.tt55007a, TT.tt55007b, TT.tt55008a, TT.tt55008b, TT.tt55009a, TT.tt55009b, TT.tt55010a, TT.tt55010b, TT.tt55011a, TT.tt55011b, TT.tt55012, TT.tt55013, TT.tt55014, TT.tt55015, TT.tt55016, TT.tt55034a, TT.tt55034b, TT.tt55035, TT.tt55036, TT.tt55062
  , aos50068, aos50069, aos50070, aos50071, aos50092, aos50114, aos50115, aos50116, aos50117, aos50118, aos50119, aos50132, aos50144, aos50145, aos50149, aos50153, aos50157, aos50158, aos50170, aos50171, aos50181b, aos50182b, aos50183b
  , FalconWinter.fw53039, FalconWinter.fw53040, FalconWinter.fw54030, FalconWinter.fw54035
  , BlackPantherShuriPack.energyTruncheonShuri
  , nc48029
  , pyrosFlamethrower
  , lifeDrain
  , Jubilee.jubilee47026, Jubilee.jubilee47027
  , prelateSidearm, prelateArmor, geneticExperiments, goldenHorse, metalWings, ahabsEnergySpear, noLongerWorthy, cyberpathy, biomorphing, molecularControl, theFittest, highTechGoggles, geneticEnhancement, cruelExperiment, escapedMutant, imperialGuardsman, staggeringStrength, celestialArmor, celestialWeapon
  , tabulaRasa16, antiRegenerationRay, poolIzed
  , cybermods, criticalWound
  , harpoonsHarpoon
  , psionicIllusion
  , hood24019, hood24020, hood24021, hood24022, hood24037, hood24038, hood24039, hood24040
      , ne40082, ne40083, ne40090, ne40091, ne40092, ne40105a, ne40105b, ne40106, ne40107, ne40122a, ne40122b, ne40123, ne40144, ne40151, ne40152, ne40153, ne40155, ne40156, ne40157, ne40159, ne40169, ne40170, ne40171, ne40172, ne40173
  , superpowerFeedback
  , cable40034
  , mojo39005, mojo39006, mojo39018, mojo39027, mojo39028, mojo39048, mojo39067
  , cyberneticEnhancements
  , psionicShield
  , switchblade, possessed
  , geneTherapy
  , deathFactor, adamantiumUpgrades
  , mgMagnetosPower
  , mgAdamantiumClaws, mgAnimalFerocity, mgHomoSuperior, mgGauntletBeam, mgLearningAI, mgAdaptiveArmor, mgEnergyBarrier, mgTargetedForElimination, mgUnitUpgrade, mgStunBeam, mgPhysicalStrain, mgMagnetosHelmet, mgMagnetosArmor, mgMagneticBubble, mgWrappedInMetal, mgNanoSentinelTech, smSandForm, smNowWeReAngry, smHumongousHallucination, smMasterfulMirage, smHeightenedMorale, smTauntingPresence, smTeamLeader, smTakeOneForTheTeam, smWeAreOne, smAdvancedGlider, smConcussiveBombs, smIncendiaryBombs, smSmokeBombs, smArmCannon, smIonicBoots, smKineticArmor, smNeocarbonScales, smSpikedGauntlet, smTrackingDisplay, smInducedPanic, smImprovisedWeapons, smViolentTendencies, smWebbedUp, smDelusionOfCollusion, smManipulatedMind, smOldGrudge, smSnitchesGetStitches
  , establishedDominance
      , theHoodsMantle
      , theHoodsPistol
      , armoredRhinoSuit
      , charge
      , enhancedIvoryHorn
      , sonicConverter
      , solidSoundBody
      , programTransmitter
      , geneticallyEnhanced
      , upgradedDrones
      , vibraniumArmor
      , concussionBlasters
      , biomechanicalUpgrades
      , counterspell
      -- The Green Goblin
      , goblinGlider
      , pumpkinBombs
      , allTiedUp
      , mediaCoverage
      , goblinGliderMutagenFormula
      , hysteria
      , pumpkinBombsMutagenFormula

      -- The Wrecking Crew
      , heldHostage
      , heldHostageThunderball
      , heldHostagePiledriver
      , heldHostageBulldozer
      , magicCrowbar
      , wreckersCommand
      , ballAndChain
      , radioactiveBuildup
      , distractingTaunts
      , bulldozersHelmet
      , rammingSpeed

      -- The Rise of Red Skull
      , crossfiresRifle
      , crossbonesMachineGun
      , crossbonesArmor
      , laserRifle
      , energyShield
      , powerGauntlets
      , exoSuit
      , absorbingMansBallAndChain
      , combatKnife
      , hydraSidearm

      -- Zola
      , defensiveProgramming
      , painInhibitors
      , neurologicalImplants

      -- Red Skull
      , redSkullsLuger
      , redSkullsRightHook
      , masterStrategist
      , twistedReality

      -- Taskmaster
      , taskmastersSword
      , taskmastersShield
      , photographicReflexes

      -- The Once and Future Kang
      , temporalShield
      , futureWeapon
      , frozenInTime


      -- Ant-Man
      , sizeIncrease

      -- Wasp
      , beetleArmorMkIV

      -- Quicksilver
      , vibrationResistance

      -- Scarlet Witch
      , magicalSuspension

      -- Gamora
      , inABind

      -- Rocket Raccoon
      , blackjacksBazooka

      -- The Galaxy's Most Wanted: Power Stone
      , powerStone
      -- Brotherhood of Badoon
      , drangsSpear
      -- Ronan the Accuser
      , universalWeapon
      , fanaticism
      -- Kree Militants
      , kreeCombatArmor

      -- The Galaxy's Most Wanted: Nebula
      , cutthroatAmbition
      , evasiveManeuvering
      , unyieldingPersistence
      , weaponMastery
      , wideStance

      -- The Galaxy's Most Wanted: Infiltrate the Museum
      , biogramImage
      -- Galactic Artifacts
      , cloakOfHercules
      , obediencePotion
      , theBeyondersBlazer
      , thePoison
      , vandarianPowerWand
      -- Drax
      , challengeAccepted
      -- Spectrum
      , sapPower

      -- The Mad Titan's Shadow: Ebony Maw
      , abjuration
      , restrained

      -- The Mad Titan's Shadow: Tower Defense
      , focusedDefense
      , proximasSpear
      , corvussGlaive
      , directAssault

      -- The Mad Titan's Shadow: Thanos
      , thanossArmor
      , thanossHelmet
      , masterOfTheStones
      -- Infinity Gauntlet
      , infinityGauntlet
      -- The Mad Titan's Shadow: Loki
      , lokisStaff
      , lokisCrown
      , lokisCape
      , masterOfIllusions

      -- The Mad Titan's Shadow: Hela
      , nightsword
      , helasCrown
      , helasCloak
      -- Legions of Hel
      , fallenWarrior
      -- Frost Giants
      , frozen
      -- Enchantress
      , beguiled
      , seduced

      -- The Mad Titan's Shadow: the campaign set
      , jormungand
      , lethalWeapon
      , beguiledValkyrie
      , seducedValkyrie
  , experimentalInjection
  , razorClaws
  , rollinRollin
  , cyborgTech
  , haywire
  , gobblerGlider
  , unstoppable
  , telepathicRestraint
  ]

attachment ::
  CardCode ->
  Name ->
  [Trait] ->
  [BoostIcon] ->
  EncounterSet ->
  Natural ->
  CardDef
attachment code name traits boostIcons encounterSet quantity =
  CardDef
    { cdCardCode = code
    , cdName = name
    , cdCost = Nothing
    , cdTraits = fromList traits
    , cdKeywords = mempty
    , cdCardType = AttachmentType
    , cdAbilityType = Nothing
    , cdAbilitySubTypes = []
    , cdUnique = False
    , cdAspect = Nothing
    , cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just quantity
    , cdCriteria = NoCriteria
    , cdResources = []
    , cdBoostIcons = boostIcons
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

unique :: CardDef -> CardDef
unique def = def {cdUnique = True}

armoredRhinoSuit :: CardDef
armoredRhinoSuit = attachment "01098" "Armored Rhino Suit" [Armor] [] Rhino 1

charge :: CardDef
charge = attachment "01099" "Charge" [Weapon] [Boost, Boost] Rhino 2

enhancedIvoryHorn :: CardDef
enhancedIvoryHorn =
  attachment "01100" "Enhanced Ivory Horn" [Weapon] [Boost, Boost] Rhino 1

sonicConverter :: CardDef
sonicConverter =
  unique $
    attachment "01118" "Sonic Converter" [Weapon] [Boost, Boost, Boost] Klaw 1

solidSoundBody :: CardDef
solidSoundBody =
  attachment "01119" "Solid-Sound Body" [Condition] [Boost, Boost, Boost] Klaw 1

programTransmitter :: CardDef
programTransmitter =
  attachment "01141" "Program Transmitter" [Item, Tech] [Boost] Ultron 1

geneticallyEnhanced :: CardDef
geneticallyEnhanced =
  attachment "01163" "Genetically Enhanced" [Condition] [Boost] SheHulkNemesis 1

upgradedDrones :: CardDef
upgradedDrones = attachment "01142" "Upgraded Drones" [Condition] [] Ultron 2

vibraniumArmor :: CardDef
vibraniumArmor = attachment "01152" "Vibranium Armor" [Armor] [Boost] UnderAttack 1

concussionBlasters :: CardDef
concussionBlasters =
  attachment "01153" "Concussion Blasters" [Weapon] [Boost] UnderAttack 1

biomechanicalUpgrades :: CardDef
biomechanicalUpgrades =
  (attachment "01185" "Biomechanical Upgrades" [Tech] [Boost] TheDoomsdayChair 3)
    { cdKeywords = singleton Surge
    }

counterspell :: CardDef
counterspell =
  attachment "09030" "Counterspell" [Condition, Spell] [Boost] DoctorStrangeNemesis 2

crossfiresRifle :: CardDef
crossfiresRifle = (attachment "04029" "Crossfire's Rifle" [Weapon] [Boost, Boost] HawkeyeNemesis 1) {cdUnique = True}
crossbonesMachineGun :: CardDef
crossbonesMachineGun =
  unique $ attachment "04064" "Crossbones' Machine Gun" [Weapon] [Boost, Boost, Boost] Crossbones 1

crossbonesArmor :: CardDef
crossbonesArmor =
  unique $ attachment "04065" "Crossbones' Armor" [Armor] [Boost, Boost, Boost] Crossbones 1

laserRifle :: CardDef
laserRifle = attachment "04072" "Laser Rifle" [Experimental, Weapon] [Boost, Boost] ExperimentalWeapons 1

energyShield :: CardDef
energyShield = attachment "04073" "Energy Shield" [Experimental, Weapon] [Boost, Boost] ExperimentalWeapons 1

powerGauntlets :: CardDef
powerGauntlets = attachment "04074" "Power Gauntlets" [Experimental, Weapon] [Boost, Boost] ExperimentalWeapons 1

exoSuit :: CardDef
exoSuit = attachment "04075" "Exo-Suit" [Experimental, Weapon] [Boost, Boost] ExperimentalWeapons 1

-- | Absorbing Man's; Thunderball has a Ball and Chain of his own.
absorbingMansBallAndChain :: CardDef
absorbingMansBallAndChain = unique $ attachment "04084" "Ball and Chain" [Weapon] [Star] AbsorbingMan 1

combatKnife :: CardDef
combatKnife = attachment "04148" "Combat Knife" [Weapon] [Boost] WeaponMaster 1

hydraSidearm :: CardDef
hydraSidearm = attachment "04149" "Hydra Sidearm" [Weapon] [Boost, Boost] WeaponMaster 1

-- The Rise of Red Skull
-- Zola

defensiveProgramming :: CardDef
defensiveProgramming = attachment "04117" "Defensive Programming" [Tech] [Boost] Zola 3

painInhibitors :: CardDef
painInhibitors = attachment "04118" "Pain Inhibitors" [Tech] [Boost] Zola 2

neurologicalImplants :: CardDef
neurologicalImplants = attachment "04119" "Neurological Implants" [Tech] [Boost] Zola 2

-- Red Skull

redSkullsLuger :: CardDef
redSkullsLuger = unique $ attachment "04132" "Red Skull's Luger" [Weapon] [Star] RedSkull 1

redSkullsRightHook :: CardDef
redSkullsRightHook = attachment "04133" "Red Skull's Right Hook" [Skill] [Boost, Boost, Boost] RedSkull 2

masterStrategist :: CardDef
masterStrategist = attachment "04134" "Master Strategist" [Skill] [Boost, Boost] RedSkull 2

twistedReality :: CardDef
twistedReality =
  (attachment "04135" "Twisted Reality" [Condition] [Boost, Boost] RedSkull 2)
    { cdKeywords = singleton (Incite 1)
    }

-- The Green Goblin

goblinGlider :: CardDef
goblinGlider = attachment "02033" "Goblin Glider" [Vehicle] [Boost, Boost, Boost] GoblinGimmicks 2

pumpkinBombs :: CardDef
pumpkinBombs = attachment "02034" "Pumpkin Bombs" [Weapon] [Boost, Boost] GoblinGimmicks 2

allTiedUp :: CardDef
allTiedUp = attachment "02048" "All Tied Up" [] [] RunningInterference 1

mediaCoverage :: CardDef
mediaCoverage = attachment "02049" "Media Coverage" [] [Boost] RunningInterference 2

goblinGliderMutagenFormula :: CardDef
goblinGliderMutagenFormula =
  (reprint "02019" goblinGlider) {cdEncounterSet = Just MutagenFormula, cdEncounterSetQuantity = Just 1}

hysteria :: CardDef
hysteria = attachment "02020" "Hysteria" [Condition] [Boost] MutagenFormula 1

pumpkinBombsMutagenFormula :: CardDef
pumpkinBombsMutagenFormula =
  (reprint "02021" pumpkinBombs) {cdEncounterSet = Just MutagenFormula, cdEncounterSetQuantity = Just 1}

-- The Rise of Red Skull

-- Taskmaster

taskmastersSword :: CardDef
taskmastersSword = unique $ attachment "04102" "Taskmaster's Sword" [Weapon] [Boost, Boost, Boost] Taskmaster 1

taskmastersShield :: CardDef
taskmastersShield = unique $ attachment "04103" "Taskmaster's Shield" [Armor, Weapon] [Boost, Boost, Boost] Taskmaster 1

photographicReflexes :: CardDef
photographicReflexes = attachment "04104" "Photographic Reflexes" [] [Boost, Boost] Taskmaster 2

-- The Wrecking Crew

heldHostage :: CardDef
heldHostage = attachment "07005" "Held Hostage" [] [Boost] Wrecker 1

-- | Held Hostage, one in each of the Wrecking Crew's decks.
heldHostageThunderball :: CardDef
heldHostageThunderball = (reprint "07021" heldHostage) {cdEncounterSet = Just Thunderball}

heldHostagePiledriver :: CardDef
heldHostagePiledriver = (reprint "07036" heldHostage) {cdEncounterSet = Just Piledriver}

heldHostageBulldozer :: CardDef
heldHostageBulldozer = (reprint "07050" heldHostage) {cdEncounterSet = Just Bulldozer}

magicCrowbar :: CardDef
magicCrowbar = unique $ attachment "07006" "Magic Crowbar" [] [Boost, Boost, Boost] Wrecker 1

wreckersCommand :: CardDef
wreckersCommand = unique $ attachment "07007" "Wrecker's Command" [] [Boost] Wrecker 1

ballAndChain :: CardDef
ballAndChain = unique $ attachment "07020" "Ball and Chain" [Weapon] [Boost, Boost, Boost] Thunderball 1

radioactiveBuildup :: CardDef
radioactiveBuildup = attachment "07022" "Radioactive Buildup" [Condition] [Boost, Boost, Boost] Thunderball 1

distractingTaunts :: CardDef
distractingTaunts = attachment "07035" "Distracting Taunts" [] [Boost, Boost] Piledriver 2

bulldozersHelmet :: CardDef
bulldozersHelmet = unique $ attachment "07049" "Bulldozer's Helmet" [Armor] [Boost, Boost, Boost] Bulldozer 1

rammingSpeed :: CardDef
rammingSpeed = attachment "07051" "Ramming Speed" [Condition] [Boost, Boost] Bulldozer 2

-- The Once and Future Kang

temporalShield :: CardDef
temporalShield = attachment "11014" "Temporal Shield" [Tech] [Boost] Kang 2

futureWeapon :: CardDef
futureWeapon = attachment "11015" "Future Weapon" [Tech, Weapon] [Boost, Boost] Kang 2

frozenInTime :: CardDef
frozenInTime = attachment "11016" "Frozen in Time" [Trait.Temporal] [Star] Kang 1

-- Ant-Man

sizeIncrease :: CardDef
sizeIncrease = attachment "12028" "Size Increase" [Condition] [] AntManNemesis 2

-- Wasp

beetleArmorMkIV :: CardDef
beetleArmorMkIV = unique $ attachment "13029" "Beetle Armor MK IV" [Armor, Tech] [Boost, Boost] WaspNemesis 1

-- Quicksilver

vibrationResistance :: CardDef
vibrationResistance = attachment "14027" "Vibration Resistance" [Condition] [Boost] QuicksilverNemesis 1

-- Scarlet Witch

magicalSuspension :: CardDef
magicalSuspension = attachment "15026" "Magical Suspension" [Spell] [Boost, Boost] ScarletWitchNemesis 1

-- Gamora

-- | Attach to Gamora.
inABind :: CardDef
inABind = attachment "18027" "In a Bind" [Armor, Tech] [Boost] GamoraNemesis 1

-- Rocket Raccoon

-- | Attach to Blackjack O'Hare, if able; if not, to the villain.
blackjacksBazooka :: CardDef
blackjacksBazooka = attachment "16056" "Blackjack's Bazooka" [Tech, Weapon] [Boost, Boost] RocketRaccoonNemesis 1

-- The Galaxy's Most Wanted: Power Stone

-- | Setup. Attach to the villain. Permanent.
powerStone :: CardDef
powerStone = (unique $ attachment "16149" "Power Stone" [InfinityStone] [] PowerStone 1) {cdKeywords = fromList [Setup, Permanent]}

-- The Galaxy's Most Wanted: Brotherhood of Badoon

drangsSpear :: CardDef
drangsSpear = unique $ attachment "16064" "Drang's Spear" [Weapon] [Boost, Boost, Boost] BrotherhoodOfBadoon 1

-- Ronan the Accuser

-- | Attach to Ronan the Accuser.
universalWeapon :: CardDef
universalWeapon = unique $ attachment "16109" "Universal Weapon" [Weapon] [Star] Ronan 1

-- | Surge. Attach to Ronan the Accuser. +1 ATK.
fanaticism :: CardDef
fanaticism = (attachment "16110" "Fanaticism" [Condition] [Boost, Boost] Ronan 2) {cdKeywords = singleton Surge}

-- Kree Militants

-- | Attach to the enemy with the highest ATK.
kreeCombatArmor :: CardDef
kreeCombatArmor = attachment "16131" "Kree Combat Armor" [Armor] [Boost, Boost] KreeMilitants 1

-- The Galaxy's Most Wanted: Nebula

-- | A Technique: attach to Nebula; its "Special" ability and its [star] boost.
technique :: CardCode -> Name -> Natural -> CardDef
technique code name = attachment code name [Technique] [Star] Nebula

cutthroatAmbition :: CardDef
cutthroatAmbition = technique "16094" "Cutthroat Ambition" 2

evasiveManeuvering :: CardDef
evasiveManeuvering = technique "16095" "Evasive Maneuvering" 1

unyieldingPersistence :: CardDef
unyieldingPersistence = technique "16096" "Unyielding Persistence" 1

weaponMastery :: CardDef
weaponMastery = technique "16097" "Weapon Mastery" 2

wideStance :: CardDef
wideStance = technique "16098" "Wide Stance" 2

-- The Galaxy's Most Wanted: Infiltrate the Museum

-- | Attach to Collector.
biogramImage :: CardDef
biogramImage = attachment "16074" "Biogram Image" [Illusion] [Star, Boost] InfiltrateTheMuseum 1

-- Galactic Artifacts: the printed cards' stat icons, hazard and amplify icons and names
-- (MarvelCDB leaves them out, and names the wand a stone)

-- | Attach to the enemy with the lowest ATK; +3 ATK.
cloakOfHercules :: CardDef
cloakOfHercules = unique $ attachment "16122" "Cloak of Hercules" [Artifact] [Boost, Boost] GalacticArtifacts 1

-- | Attach to your identity.
obediencePotion :: CardDef
obediencePotion = unique $ attachment "16123" "Obedience Potion" [Artifact] [Boost, Boost] GalacticArtifacts 1

-- | Attach to the enemy with the highest SCH; one amplify icon.
theBeyondersBlazer :: CardDef
theBeyondersBlazer = (unique $ attachment "16124" "The Beyonder's Blazer" [Artifact] [Boost, Boost, Boost] GalacticArtifacts 1) {cdAmplify = 1}

-- | Attach to your identity.
thePoison :: CardDef
thePoison = unique $ attachment "16125" "The Poison" [Artifact] [Boost, Boost] GalacticArtifacts 1

-- | Attach to the enemy with the lowest SCH; one hazard icon.
vandarianPowerWand :: CardDef
vandarianPowerWand = (unique $ attachment "16126" "Vandarian Power Wand" [Artifact] [Boost] GalacticArtifacts 1) {cdHazards = 1}
-- Drax

-- | Attach to the enemy with the highest ATK.
-- boost icons from the printed card (MarvelCDB has none for this pack)
challengeAccepted :: CardDef
challengeAccepted =
  (attachment "19028" "Challenge Accepted" [] [Boost] DraxNemesis 1)
    { cdKeywords = singleton Surge
    }

-- Spectrum

sapPower :: CardDef
sapPower = attachment "21029" "Sap Power" [] [Boost, Boost] SpectrumNemesis 2

-- The Mad Titan's Shadow: Tower Defense

-- | Permanent. Put into play by The Armies of Thanos (2A), so none is shuffled into the
-- encounter deck.
focusedDefense :: CardDef
focusedDefense = (attachment "21101" "Focused Defense" [] [] TowerDefense 0) {cdKeywords = singleton Permanent}

proximasSpear :: CardDef
proximasSpear = unique $ attachment "21103" "Proxima's Spear" [Weapon] [Boost, Boost, Boost] TowerDefense 1

corvussGlaive :: CardDef
corvussGlaive = unique $ attachment "21104" "Corvus's Glaive" [Weapon] [Boost, Boost, Boost] TowerDefense 1

directAssault :: CardDef
directAssault = attachment "21105" "Direct Assault" [Tactic] [Boost, Boost] TowerDefense 2

-- The Mad Titan's Shadow: Ebony Maw

abjuration :: CardDef
abjuration = attachment "21082" "Abjuration" [] [Boost, Boost, Boost] EbonyMaw 1

restrained :: CardDef
restrained = attachment "21083" "Restrained" [] [Boost, Boost] EbonyMaw 1

-- The Mad Titan's Shadow: Thanos

-- | Attach to Thanos.
thanossArmor :: CardDef
thanossArmor = unique $ attachment "21117" "Thanos's Armor" [Armor] [Boost, Boost, Boost] Thanos 1

-- | Attach to Thanos.
thanossHelmet :: CardDef
thanossHelmet = unique $ attachment "21118" "Thanos's Helmet" [Armor] [Boost, Boost, Boost] Thanos 1

-- | Attach to Thanos: +1 ATK, +1 SCH.
masterOfTheStones :: CardDef
masterOfTheStones = attachment "21119" "Master of the Stones" [] [Boost] Thanos 2

-- Infinity Gauntlet

-- | Permanent. Setup. Attach to the villain (errata): +1 ATK, +1 SCH.
infinityGauntlet :: CardDef
infinityGauntlet = (unique $ attachment "21129" "Infinity Gauntlet" [Artifact, Weapon] [] InfinityGauntlet 1) {cdKeywords = fromList [Permanent, Setup]}

-- The Mad Titan's Shadow: Loki

lokisStaff :: CardDef
lokisStaff = unique $ attachment "21170" "Loki's Staff" [Weapon] [Star] Loki 1

lokisCrown :: CardDef
lokisCrown = unique $ attachment "21171" "Loki's Crown" [Item] [Star] Loki 1

lokisCape :: CardDef
lokisCape = unique $ attachment "21172" "Loki's Cape" [Armor] [Boost, Boost] Loki 1

masterOfIllusions :: CardDef
masterOfIllusions = attachment "21173" "Master of Illusions" [Condition] [Boost] Loki 1

-- The Mad Titan's Shadow: Hela

nightsword :: CardDef
nightsword = unique $ attachment "21146" "Nightsword" [Weapon] [Star] Hela 1

helasCrown :: CardDef
helasCrown = unique $ attachment "21147" "Hela's Crown" [Item] [Star] Hela 1

helasCloak :: CardDef
helasCloak = unique $ attachment "21148" "Hela's Cloak" [Armor] [Star] Hela 1

-- Legions of Hel

fallenWarrior :: CardDef
fallenWarrior = attachment "21153" "Fallen Warrior" [Condition] [Boost, Boost] LegionsOfHel 2

-- Frost Giants

frozen :: CardDef
frozen = attachment "21158" "Frozen" [Condition] [Boost, Boost] FrostGiants 2

-- Enchantress

beguiled :: CardDef
beguiled = attachment "21178" "Beguiled" [Condition] [Boost] Enchantress 2

seduced :: CardDef
seduced = attachment "21179" "Seduced" [Condition] [Boost, Boost] Enchantress 2

-- The Mad Titan's Shadow: the campaign set

-- | Open the Dungeons' other side.
jormungand :: CardDef
jormungand = unique $ attachment "21189b" "Jormungand" [Dragon] [] MadTitansShadowCampaign 1

-- Nebula

lethalWeapon :: CardDef
lethalWeapon = attachment "22030" "Lethal Weapon" [Weapon] [Boost, Boost] NebulaNemesis 1

-- Valkyrie

beguiledValkyrie :: CardDef
beguiledValkyrie = (reprint "25031" beguiled) {cdEncounterSet = Just ValkyrieNemesis, cdEncounterSetQuantity = Just 1}

seducedValkyrie :: CardDef
seducedValkyrie = (reprint "25032" seduced) {cdEncounterSet = Just ValkyrieNemesis, cdEncounterSetQuantity = Just 2, cdTraits = mempty}

-- The Hood

establishedDominance :: CardDef
establishedDominance = attachment "24007" "Established Dominance" [] [Boost, Boost] TheHood 1

theHoodsMantle :: CardDef
theHoodsMantle = attachment "24008" "The Hood's Mantle" [Armor] [] TheHood 1

theHoodsPistol :: CardDef
theHoodsPistol = attachment "24009" "The Hood's Pistol" [Weapon] [Star] TheHood 2


-- Sinister Motives

experimentalInjection :: CardDef
experimentalInjection = attachment "27028" "Experimental Injection" [] [] GhostSpiderNemesis 1

razorClaws :: CardDef
razorClaws = attachment "27059" "Razor Claws" [Weapon] [Boost, Boost] SpiderManMoralesNemesis 1

-- Sinister Motives

smAttachmentDef :: CardCode -> Name -> SMSet.EncounterSet -> Natural -> CardDef
smAttachmentDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = AttachmentType
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

smSandForm :: CardDef
smSandForm = (smAttachmentDef "27066" "Sand Form" SMSet.Sandman 2)
  { cdTraits = fromList [SMTrait.Condition]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smNowWeReAngry :: CardDef
smNowWeReAngry = (smAttachmentDef "27078" "\"Now We're Angry!\"" SMSet.Venom 2)
  { cdTraits = fromList [SMTrait.Condition]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smHumongousHallucination :: CardDef
smHumongousHallucination = (smAttachmentDef "27089" "Humongous Hallucination" SMSet.Mysterio 1)
  { cdTraits = fromList [SMTrait.Illusion]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smMasterfulMirage :: CardDef
smMasterfulMirage = (smAttachmentDef "27090" "Masterful Mirage" SMSet.Mysterio 2)
  { cdTraits = fromList [SMTrait.Illusion]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smHeightenedMorale :: CardDef
smHeightenedMorale = (smAttachmentDef "27103" "Heightened Morale" SMSet.SinisterSix 2)
  { cdTraits = fromList [SMTrait.Condition]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smTauntingPresence :: CardDef
smTauntingPresence = (smAttachmentDef "27104" "Taunting Presence" SMSet.SinisterSix 2)
  { cdTraits = fromList [SMTrait.Condition]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smTeamLeader :: CardDef
smTeamLeader = (smAttachmentDef "27105" "Team Leader" SMSet.SinisterSix 1)
  { cdTraits = fromList [SMTrait.Title]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smTakeOneForTheTeam :: CardDef
smTakeOneForTheTeam = (smAttachmentDef "27106" "Take One for the Team" SMSet.SinisterSix 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smWeAreOne :: CardDef
smWeAreOne = (smAttachmentDef "27120" "We Are One" SMSet.VenomGoblin 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smAdvancedGlider :: CardDef
smAdvancedGlider = (smAttachmentDef "27136" "Advanced Glider" SMSet.GoblinGear 1)
  { cdTraits = fromList [SMTrait.Item, SMTrait.Tech]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smConcussiveBombs :: CardDef
smConcussiveBombs = (smAttachmentDef "27137" "Concussive Bombs" SMSet.GoblinGear 1)
  { cdTraits = fromList [SMTrait.Tech, SMTrait.Weapon]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smIncendiaryBombs :: CardDef
smIncendiaryBombs = (smAttachmentDef "27138" "Incendiary Bombs" SMSet.GoblinGear 1)
  { cdTraits = fromList [SMTrait.Tech, SMTrait.Weapon]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSmokeBombs :: CardDef
smSmokeBombs = (smAttachmentDef "27139" "Smoke Bombs" SMSet.GoblinGear 1)
  { cdTraits = fromList [SMTrait.Tech, SMTrait.Weapon]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smArmCannon :: CardDef
smArmCannon = (smAttachmentDef "27147" "Arm Cannon" SMSet.OsbornTech 1)
  { cdTraits = fromList [SMTrait.Tech, SMTrait.Weapon]
  , cdKeywords = fromList [SMKeyword.Surge]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smIonicBoots :: CardDef
smIonicBoots = (smAttachmentDef "27148" "Ionic Boots" SMSet.OsbornTech 1)
  { cdTraits = fromList [SMTrait.Item, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Surge]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smKineticArmor :: CardDef
smKineticArmor = (smAttachmentDef "27149" "Kinetic Armor" SMSet.OsbornTech 1)
  { cdTraits = fromList [SMTrait.Armor, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Surge]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smNeocarbonScales :: CardDef
smNeocarbonScales = (smAttachmentDef "27150" "Neocarbon Scales" SMSet.OsbornTech 1)
  { cdTraits = fromList [SMTrait.Armor, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Surge]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSpikedGauntlet :: CardDef
smSpikedGauntlet = (smAttachmentDef "27151" "Spiked Gauntlet" SMSet.OsbornTech 1)
  { cdTraits = fromList [SMTrait.Tech, SMTrait.Weapon]
  , cdKeywords = fromList [SMKeyword.Surge]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smTrackingDisplay :: CardDef
smTrackingDisplay = (smAttachmentDef "27152" "Tracking Display" SMSet.OsbornTech 1)
  { cdTraits = fromList [SMTrait.Item, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Surge]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smInducedPanic :: CardDef
smInducedPanic = (smAttachmentDef "27153" "Induced Panic" SMSet.PersonalNightmare 1)
  { cdTraits = fromList [SMTrait.Illusion]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smImprovisedWeapons :: CardDef
smImprovisedWeapons = (smAttachmentDef "27164" "Improvised Weapons" SMSet.SymbioticStrength 1)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 1
  }

smViolentTendencies :: CardDef
smViolentTendencies = (smAttachmentDef "27165" "Violent Tendencies" SMSet.SymbioticStrength 1)
  { cdTraits = fromList [SMTrait.Condition]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smWebbedUp :: CardDef
smWebbedUp = (smAttachmentDef "27166" "Webbed Up" SMSet.SymbioticStrength 1)
  { cdTraits = fromList [SMTrait.Condition]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smDelusionOfCollusion :: CardDef
smDelusionOfCollusion = (smAttachmentDef "27170" "Delusion of Collusion" SMSet.WhispersOfParanoia 2)
  { cdTraits = fromList [SMTrait.Illusion]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smManipulatedMind :: CardDef
smManipulatedMind = (smAttachmentDef "27171" "Manipulated Mind" SMSet.WhispersOfParanoia 1)
  { cdTraits = fromList [SMTrait.Illusion]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smOldGrudge :: CardDef
smOldGrudge = (smAttachmentDef "27172" "Old Grudge" SMSet.WhispersOfParanoia 1)
  { cdTraits = fromList [SMTrait.Illusion]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Star]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSnitchesGetStitches :: CardDef
smSnitchesGetStitches = (smAttachmentDef "27181" "Snitches Get Stitches" SMSet.SnitchesGetStitches 1)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Victory (-1)]
  , cdUnique = False
  , cdBoostIcons = [SMBoost.Boost, SMBoost.Boost, SMBoost.Boost]
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

-- Nova

rollinRollin :: CardDef
rollinRollin = attachment "28030" "Rollin', Rollin'" [Condition] [Boost, Boost] SMSet.Armadillo 1


-- Ironheart

cyborgTech :: CardDef
cyborgTech = (attachment "29031" "Cyborg Tech" [Tech] [Star] SMSet.IronheartNemesis 1)
  { cdUnique = False
  }

haywire :: CardDef
haywire = (attachment "29038" "Haywire" [] [Boost] SMSet.ZzzaxSet 2)
  { cdUnique = False
  }


-- Spider-Ham

gobblerGlider :: CardDef
gobblerGlider = (attachment "30027" "Gobbler Glider" [] [Boost, Boost] SMSet.SpiderHamNemesis 1)
  { cdUnique = False
  , cdKeywords = fromList [Surge]
  }

-- Mutant Genesis

unstoppable :: CardDef
unstoppable = (attachment "32028" "Unstoppable" [] [Boost, Boost] ColossusNemesis 2)

-- Mutant Genesis

telepathicRestraint :: CardDef
telepathicRestraint = (attachment "32059" "Telepathic Restraint" [Condition] [Boost, Boost] ShadowcatNemesis 1)

-- Mutant Genesis scenario definitions
mgAdamantiumClaws :: CardDef
mgAdamantiumClaws = (smAttachmentDef "32067" ("Adamantium Claws") MGSet.Sabretooth 1)
  { cdTraits = fromList [MGTrait.Weapon]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList []
  }

mgAnimalFerocity :: CardDef
mgAnimalFerocity = (smAttachmentDef "32068" ("Animal Ferocity") MGSet.Sabretooth 1)
  { cdTraits = fromList [MGTrait.Condition]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList []
  }

mgHomoSuperior :: CardDef
mgHomoSuperior = (smAttachmentDef "32077" ("Homo Superior") MGSet.Brotherhood 2)
  { cdTraits = fromList [MGTrait.Condition]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Star]
  , cdKeywords = fromList []
  }

mgGauntletBeam :: CardDef
mgGauntletBeam = (smAttachmentDef "32094" ("Gauntlet Beam") MGSet.ProjectWideawake 2)
  { cdTraits = fromList [MGTrait.Sentinel, MGTrait.Tech]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Star]
  , cdKeywords = fromList []
  }

mgLearningAI :: CardDef
mgLearningAI = (smAttachmentDef "32095" ("Learning A.I.") MGSet.ProjectWideawake 1)
  { cdTraits = fromList [MGTrait.Sentinel, MGTrait.Tech]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList []
  }

mgAdaptiveArmor :: CardDef
mgAdaptiveArmor = (smAttachmentDef "32096" ("Adaptive Armor") MGSet.ProjectWideawake 1)
  { cdTraits = fromList [MGTrait.Sentinel, MGTrait.Tech]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList []
  }

mgEnergyBarrier :: CardDef
mgEnergyBarrier = (smAttachmentDef "32103" ("Energy Barrier") MGSet.ZeroTolerance 2)
  { cdTraits = fromList [MGTrait.Sentinel, MGTrait.Tech]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgTargetedForElimination :: CardDef
mgTargetedForElimination = (smAttachmentDef "32107" ("Targeted for Elimination") MGSet.Sentinels 2)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgUnitUpgrade :: CardDef
mgUnitUpgrade = (smAttachmentDef "32115" ("Unit Upgrade") MGSet.MasterMold 2)
  { cdTraits = fromList [MGTrait.Sentinel, MGTrait.Tech]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Star]
  , cdKeywords = fromList []
  }

mgStunBeam :: CardDef
mgStunBeam = (smAttachmentDef "32116" ("Stun Beam") MGSet.MasterMold 2)
  { cdTraits = fromList [MGTrait.Sentinel, MGTrait.Tech]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgPhysicalStrain :: CardDef
mgPhysicalStrain = (smAttachmentDef "32145b" ("Physical Strain") MGSet.Magneto 0)
  { cdTraits = fromList [MGTrait.Condition]
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList [MGKeyword.Permanent]
  }

mgMagnetosHelmet :: CardDef
mgMagnetosHelmet = (smAttachmentDef "32147" ("Magneto's Helmet") MGSet.Magneto 1)
  { cdTraits = fromList [MGTrait.Armor, MGTrait.Magnetic]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgMagnetosArmor :: CardDef
mgMagnetosArmor = (smAttachmentDef "32148" ("Magneto's Armor") MGSet.Magneto 1)
  { cdTraits = fromList [MGTrait.Armor, MGTrait.Magnetic]
  , cdUnique = True
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgMagneticBubble :: CardDef
mgMagneticBubble = (smAttachmentDef "32149" ("Magnetic Bubble") MGSet.Magneto 1)
  { cdTraits = fromList [MGTrait.Magnetic]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgWrappedInMetal :: CardDef
mgWrappedInMetal = (smAttachmentDef "32150" ("Wrapped in Metal") MGSet.Magneto 1)
  { cdTraits = fromList [MGTrait.Magnetic]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList []
  }

mgNanoSentinelTech :: CardDef
mgNanoSentinelTech = (smAttachmentDef "32170" ("Nano-Sentinel Tech") MGSet.FuturePast 1)
  { cdTraits = fromList [MGTrait.Sentinel, MGTrait.Tech]
  , cdUnique = False
  , cdBoostIcons = [MGBoost.Boost, MGBoost.Boost]
  , cdKeywords = fromList [MGKeyword.Victory 1]
  }


mgMagnetosPower :: CardDef
mgMagnetosPower = (attachment "32175b" "Magneto's Power" [MGTrait.Condition] [] MGSet.MutantGenesisCampaign 1) {cdKeywords = fromList [MGKeyword.Permanent]}

-- Wolverine

deathFactor :: CardDef
deathFactor = attachment "35030" "Death Factor" [] [Boost, Boost] WolverineNemesis 2

adamantiumUpgrades :: CardDef
adamantiumUpgrades = attachment "35036" "Adamantium Upgrades" [Tech] [Boost, Boost] LadyDeathstrikeSet 2


-- Cyclops

geneTherapy :: CardDef
geneTherapy = (attachment "33030" "Gene Therapy" [] [Boost, Boost] CyclopsNemesis 2)


-- Storm

switchblade :: CardDef
switchblade = attachment "36033" "Switchblade" [] [Boost, Boost] StormNemesis 1

possessed :: CardDef
possessed = attachment "36038" "Possessed" [Condition] [Boost, Boost] ShadowKingSet 2

-- Gambit

psionicShield :: CardDef
psionicShield = attachment "37034" "Psionic Shield" [Psionic] [Boost, Boost] ExodusSet 2


-- Rogue

cyberneticEnhancements :: CardDef
cyberneticEnhancements = attachment "38035" "Cybernetic Enhancements" [Tech] [Boost, Boost] ReaversSet 2


-- Mojo Mania

mojoCardDef :: CardCode -> Name -> MojoSet.EncounterSet -> Natural -> CardDef
mojoCardDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = AttachmentType
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

mojo39005 :: CardDef
mojo39005 = (mojoCardDef "39005" "Jolt of Adrenaline" MojoSet.Magog 1)
  { cdTraits = fromList [MojoTrait.Condition]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Star]
  }

mojo39006 :: CardDef
mojo39006 = (mojoCardDef "39006" "Surge of Aggression" MojoSet.Magog 1)
  { cdTraits = fromList [MojoTrait.Condition]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Star]
  }

mojo39018 :: CardDef
mojo39018 = (mojoCardDef "39018" "Spiral's Swords" MojoSet.Spiral 2)
  { cdTraits = fromList [MojoTrait.Weapon]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39027 :: CardDef
mojo39027 = (mojoCardDef "39027" "Major Domo" MojoSet.Mojo 1)
  { cdTraits = fromList [MojoTrait.Persona]
  , cdUnique = True
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost, MojoBoost.Boost]
  }

mojo39028 :: CardDef
mojo39028 = (mojoCardDef "39028" "Stinger Tail" MojoSet.Mojo 1)
  { cdTraits = fromList [MojoTrait.Weapon]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Star]
  }

mojo39048 :: CardDef
mojo39048 = (mojoCardDef "39048" "Bandolier of Stakes" MojoSet.Horror 1)
  { cdTraits = fromList [MojoTrait.Item]
  , cdUnique = False
  , cdKeywords = fromList [MojoKeyword.Surge]
  , cdBoostIcons = []
  }

mojo39067 :: CardDef
mojo39067 = (mojoCardDef "39067" "Dead or Alive" MojoSet.Western 1)
  { cdTraits = fromList [MojoTrait.Bounty]
  , cdUnique = False
  , cdKeywords = fromList []
  , cdBoostIcons = [MojoBoost.Boost, MojoBoost.Boost]
  }

-- NeXt Evolution: Cable

cable40034 :: CardDef
cable40034 = (attachment "40034" "Telekinetic Force Field" [NE.Psionic] [NE.Boost, NE.Star] CableNemesis 1) { cdUnique = False, cdDeckLimit = Just 1 }

-- NeXt Evolution: Domino
superpowerFeedback :: CardDef
superpowerFeedback = CardDef
  { cdCardCode = "40069", cdName = "Superpower Feedback", cdCost = Nothing
  , cdTraits = fromList [NET.Condition], cdKeywords = fromList []
  , cdCardType = AttachmentType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [], cdBoostIcons = [NEB.Boost]
  , cdEncounterSet = Just NES.DominoNemesis, cdEncounterSetQuantity = Just 2
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution campaign and scenarios

ne40082 :: CardDef
ne40082 = CardDef
  { cdCardCode = "40082"
  , cdName = "Bolstered by Wrath"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Star]
  , cdEncounterSet = Just NESet.MorlockSiege
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

ne40083 :: CardDef
ne40083 = CardDef
  { cdCardCode = "40083"
  , cdName = "Pushed to the Limit"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MorlockSiege
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

ne40090 :: CardDef
ne40090 = CardDef
  { cdCardCode = "40090"
  , cdName = "Heavy Armament"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Weapon]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MilitaryGrade
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

ne40091 :: CardDef
ne40091 = CardDef
  { cdCardCode = "40091"
  , cdName = "Titanium Exoskeleton"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Armor, NETrait.Tech]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.MilitaryGrade
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

ne40092 :: CardDef
ne40092 = CardDef
  { cdCardCode = "40092"
  , cdName = "Inhibitor Collar"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Tech]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.MilitaryGrade
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

ne40105a :: CardDef
ne40105a = CardDef
  { cdCardCode = "40105a"
  , cdName = "Hope's Captor"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Confident]
  , cdKeywords = fromList [NEKeyword.Permanent]
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.OnTheRun
  , cdEncounterSetQuantity = Nothing
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

ne40105b :: CardDef
ne40105b = CardDef
  { cdCardCode = "40105b"
  , cdName = "Hope's Captor"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Desperate]
  , cdKeywords = fromList [NEKeyword.Permanent]
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.OnTheRun
  , cdEncounterSetQuantity = Nothing
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

ne40106 :: CardDef
ne40106 = CardDef
  { cdCardCode = "40106"
  , cdName = "Hidden in the Clutter"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.OnTheRun
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

ne40107 :: CardDef
ne40107 = CardDef
  { cdCardCode = "40107"
  , cdName = "Favored Weapon"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Weapon]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.OnTheRun
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

ne40122a :: CardDef
ne40122a = CardDef
  { cdCardCode = "40122a"
  , cdName = "Juggernaut's Helmet"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Armor]
  , cdKeywords = fromList [NEKeyword.Permanent]
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeJuggernaut
  , cdEncounterSetQuantity = Nothing
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

ne40122b :: CardDef
ne40122b = CardDef
  { cdCardCode = "40122b"
  , cdName = "Juggernaut Exposed"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList [NEKeyword.Permanent]
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NeJuggernaut
  , cdEncounterSetQuantity = Nothing
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

ne40123 :: CardDef
ne40123 = CardDef
  { cdCardCode = "40123"
  , cdName = "Head of Steam"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
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

ne40144 :: CardDef
ne40144 = CardDef
  { cdCardCode = "40144"
  , cdName = "Sinister Disguise"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
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

ne40151 :: CardDef
ne40151 = CardDef
  { cdCardCode = "40151"
  , cdName = "Flight"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList [NEKeyword.Permanent, NEKeyword.Setup]
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.FlightSet
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

ne40152 :: CardDef
ne40152 = CardDef
  { cdCardCode = "40152"
  , cdName = "Aerial Bombardment"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.FlightSet
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

ne40153 :: CardDef
ne40153 = CardDef
  { cdCardCode = "40153"
  , cdName = "Out of Reach"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.FlightSet
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


ne40155 :: CardDef
ne40155 = CardDef
  { cdCardCode = "40155"
  , cdName = "Super Strength"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList [NEKeyword.Permanent, NEKeyword.Setup]
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.SuperStrengthSet
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

ne40156 :: CardDef
ne40156 = CardDef
  { cdCardCode = "40156"
  , cdName = "Impervious"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.SuperStrengthSet
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

ne40157 :: CardDef
ne40157 = CardDef
  { cdCardCode = "40157"
  , cdName = "Thrown Object"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Item]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.SuperStrengthSet
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

ne40159 :: CardDef
ne40159 = CardDef
  { cdCardCode = "40159"
  , cdName = "Telepathy"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList [NEKeyword.Permanent, NEKeyword.Setup]
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.TelepathySet
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

ne40169 :: CardDef
ne40169 = CardDef
  { cdCardCode = "40169"
  , cdName = "Mental Transferal"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition, NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
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

ne40170 :: CardDef
ne40170 = CardDef
  { cdCardCode = "40170"
  , cdName = "Mind Alteration"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition, NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
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

ne40171 :: CardDef
ne40171 = CardDef
  { cdCardCode = "40171"
  , cdName = "Mind Trap"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition, NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
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

ne40172 :: CardDef
ne40172 = CardDef
  { cdCardCode = "40172"
  , cdName = "Psionic Amnesia"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition, NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
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

ne40173 :: CardDef
ne40173 = CardDef
  { cdCardCode = "40173"
  , cdName = "Psychic Inertia"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition, NETrait.Psionic]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost]
  , cdEncounterSet = Just NESet.NeStryfe
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

hood24019 :: CardDef
hood24019 = (hoodCardDef "24019" "Blackbird Pellets" HoodSet.BrothersGrimm AttachmentType 1)
  { cdTraits = fromList [HoodTrait.Item]
  , cdKeywords = fromList []
  , cdBoostIcons = []
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24020 :: CardDef
hood24020 = (hoodCardDef "24020" "Corrosive Egg Bomb" HoodSet.BrothersGrimm AttachmentType 1)
  { cdTraits = fromList [HoodTrait.Item]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24021 :: CardDef
hood24021 = (hoodCardDef "24021" "Paralytic Stardust" HoodSet.BrothersGrimm AttachmentType 1)
  { cdTraits = fromList [HoodTrait.Item]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24022 :: CardDef
hood24022 = (hoodCardDef "24022" "Unbreakable Thread" HoodSet.BrothersGrimm AttachmentType 1)
  { cdTraits = fromList [HoodTrait.Item]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24037 :: CardDef
hood24037 = (hoodCardDef "24037" "Flamethrower" HoodSet.RansackedArmory AttachmentType 1)
  { cdTraits = fromList [HoodTrait.Tech, HoodTrait.Weapon]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24038 :: CardDef
hood24038 = (hoodCardDef "24038" "Holoshield Generator" HoodSet.RansackedArmory AttachmentType 1)
  { cdTraits = fromList [HoodTrait.Item, HoodTrait.Tech]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24039 :: CardDef
hood24039 = (hoodCardDef "24039" "Jetpack" HoodSet.RansackedArmory AttachmentType 1)
  { cdTraits = fromList [HoodTrait.Item, HoodTrait.Tech]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

hood24040 :: CardDef
hood24040 = (hoodCardDef "24040" "Tech Gauntlets" HoodSet.RansackedArmory AttachmentType 2)
  { cdTraits = fromList [HoodTrait.Tech, HoodTrait.Weapon]
  , cdKeywords = fromList []
  , cdBoostIcons = [HoodBoost.Boost, HoodBoost.Boost]
  , cdUnique = False
  , cdAcceleration = 0
  , cdHazards = 0
  }

-- Psylocke

psionicIllusion :: CardDef
psionicIllusion = CardDef
  { cdCardCode = "41028", cdName = "Psionic Illusion", cdCost = Nothing
  , cdTraits = fromList [PsyTrait.Psionic], cdKeywords = fromList []
  , cdCardType = PsyCard.AttachmentType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [], cdBoostIcons = [PsyBoost.Boost, PsyBoost.Boost]
  , cdEncounterSet = Just PsySet.PsylockeNemesis, cdEncounterSetQuantity = Just 1
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

-- Angel

harpoonsHarpoon :: CardDef
harpoonsHarpoon = CardDef
  { cdCardCode = "42027"
  , cdName = "Harpoon's Harpoon"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AnCard.AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
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

cybermods :: CardDef
cybermods = CardDef
  { cdCardCode = "43031"
  , cdName = "Cybermods"
  , cdCost = Nothing
  , cdTraits = fromList [AnTrait.Tech]
  , cdKeywords = fromList []
  , cdCardType = AnCard.AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AnBoost.Boost, AnBoost.Boost]
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

criticalWound :: CardDef
criticalWound = CardDef
  { cdCardCode = "43032"
  , cdName = "Critical Wound"
  , cdCost = Nothing
  , cdTraits = fromList [AnTrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AnCard.AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [AnBoost.Boost, AnBoost.Boost]
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

tabulaRasa16 :: CardDef
tabulaRasa16 = DpCard.CardDef
  { cdCardCode = "44035"
  , cdName = DpName.Name "Tabula Rasa 16" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
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

antiRegenerationRay :: CardDef
antiRegenerationRay = DpCard.CardDef
  { cdCardCode = "44040"
  , cdName = DpName.Name "Anti-Regeneration Ray" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [DpTrait.Weapon]
  , cdKeywords = fromList []
  , cdCardType = DpCard.AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [DpBoost.Boost]
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

poolIzed :: CardDef
poolIzed = DpCard.CardDef
  { cdCardCode = "44041"
  , cdName = DpName.Name "'Pool-ized" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [DpBoost.Boost, DpBoost.Boost]
  , cdEncounterSet = Just DpSet.DreadpoolSet
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

-- Age of Apocalypse campaign and scenarios

prelateSidearm :: CardDef
prelateSidearm = CardDef
  { cdCardCode = "45063"
  , cdName = "Prelate Sidearm"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Weapon]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaUnus
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

prelateArmor :: CardDef
prelateArmor = CardDef
  { cdCardCode = "45064"
  , cdName = "Prelate Armor"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Armor]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaUnus
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

geneticExperiments :: CardDef
geneticExperiments = CardDef
  { cdCardCode = "45066"
  , cdName = "Genetic Experiments"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
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

goldenHorse :: CardDef
goldenHorse = CardDef
  { cdCardCode = "45090"
  , cdName = "Golden Horse"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Item, NETrait.Tech]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaFourHorsemen
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

metalWings :: CardDef
metalWings = CardDef
  { cdCardCode = "45091"
  , cdName = "Metal Wings"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Item, NETrait.Tech]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaFourHorsemen
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

ahabsEnergySpear :: CardDef
ahabsEnergySpear = CardDef
  { cdCardCode = "45099"
  , cdName = "Ahab's Energy Spear"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Weapon]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
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

noLongerWorthy :: CardDef
noLongerWorthy = CardDef
  { cdCardCode = "45105b"
  , cdName = "No Longer Worthy"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
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

cyberpathy :: CardDef
cyberpathy = CardDef
  { cdCardCode = "45106"
  , cdName = "Cyberpathy"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaApocalypse
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

biomorphing :: CardDef
biomorphing = CardDef
  { cdCardCode = "45107"
  , cdName = "Biomorphing"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaApocalypse
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

molecularControl :: CardDef
molecularControl = CardDef
  { cdCardCode = "45108"
  , cdName = "Molecular Control"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaApocalypse
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

theFittest :: CardDef
theFittest = CardDef
  { cdCardCode = "45109"
  , cdName = "The Fittest"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaApocalypse
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

highTechGoggles :: CardDef
highTechGoggles = CardDef
  { cdCardCode = "45122"
  , cdName = "High-Tech Goggles"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Tech]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaDarkBeast
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

geneticEnhancement :: CardDef
geneticEnhancement = CardDef
  { cdCardCode = "45123"
  , cdName = "Genetic Enhancement"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaDarkBeast
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

cruelExperiment :: CardDef
cruelExperiment = CardDef
  { cdCardCode = "45124"
  , cdName = "Cruel Experiment"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Condition]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaDarkBeast
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

escapedMutant :: CardDef
escapedMutant = CardDef
  { cdCardCode = "45137"
  , cdName = "Escaped Mutant"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost, NEBoost.Boost]
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

imperialGuardsman :: CardDef
imperialGuardsman = CardDef
  { cdCardCode = "45145"
  , cdName = "Imperial Guardsman"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Title]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
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

staggeringStrength :: CardDef
staggeringStrength = CardDef
  { cdCardCode = "45149"
  , cdName = "Staggering Strength"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Superpower]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Star]
  , cdEncounterSet = Just NESet.AoaEnSabahNur
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

celestialArmor :: CardDef
celestialArmor = CardDef
  { cdCardCode = "45156"
  , cdName = "Celestial Armor"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Armor, NETrait.Celestial]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaCelestialTech
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

celestialWeapon :: CardDef
celestialWeapon = CardDef
  { cdCardCode = "45157"
  , cdName = "Celestial Weapon"
  , cdCost = Nothing
  , cdTraits = fromList [NETrait.Celestial, NETrait.Weapon]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdBoostIcons = [NEBoost.Boost, NEBoost.Boost]
  , cdEncounterSet = Just NESet.AoaCelestialTech
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

-- Iceman
pyrosFlamethrower :: CardDef
pyrosFlamethrower = IcemanPack.pyrosFlamethrower

-- Iceman
lifeDrain :: CardDef
lifeDrain = IcemanPack.lifeDrain

-- Nightcrawler
nc48029 :: CardDef
nc48029 = CardDef
  { cdCardCode = "48029"
  , cdName = "Azazel's Sword"
  , cdCost = Nothing
  , cdTraits = fromList [NcTrait.Weapon]
  , cdKeywords = fromList []
  , cdCardType = AttachmentType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = [NcBoost.Boost, NcBoost.Boost]
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

aos50068 :: CardDef
aos50068 = AosEncounter.aos50068

aos50069 :: CardDef
aos50069 = AosEncounter.aos50069

aos50070 :: CardDef
aos50070 = AosEncounter.aos50070

aos50071 :: CardDef
aos50071 = AosEncounter.aos50071

aos50092 :: CardDef
aos50092 = AosEncounter.aos50092

aos50114 :: CardDef
aos50114 = AosEncounter.aos50114

aos50115 :: CardDef
aos50115 = AosEncounter.aos50115

aos50116 :: CardDef
aos50116 = AosEncounter.aos50116

aos50117 :: CardDef
aos50117 = AosEncounter.aos50117

aos50118 :: CardDef
aos50118 = AosEncounter.aos50118

aos50119 :: CardDef
aos50119 = AosEncounter.aos50119

aos50132 :: CardDef
aos50132 = AosEncounter.aos50132

aos50144 :: CardDef
aos50144 = AosEncounter.aos50144

aos50145 :: CardDef
aos50145 = AosEncounter.aos50145

aos50149 :: CardDef
aos50149 = AosEncounter.aos50149

aos50153 :: CardDef
aos50153 = AosEncounter.aos50153

aos50157 :: CardDef
aos50157 = AosEncounter.aos50157

aos50158 :: CardDef
aos50158 = AosEncounter.aos50158

aos50170 :: CardDef
aos50170 = AosEncounter.aos50170

aos50171 :: CardDef
aos50171 = AosEncounter.aos50171

aos50181b :: CardDef
aos50181b = AosEncounter.aos50181b

aos50182b :: CardDef
aos50182b = AosEncounter.aos50182b

aos50183b :: CardDef
aos50183b = AosEncounter.aos50183b

theAccused :: CardDef
theAccused = attachment "90003" "The Accused" [Condition] [Star] KreeFanatic 1
