module Marvel.Attachment.Cards where

import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.Trait qualified as MGTrait
import Marvel.Resource qualified as MGResource
import Marvel.Boost qualified as MGBoost
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
  fromList $
    map
      (toFst toCardCode)
      [ mgMagnetosPower
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
