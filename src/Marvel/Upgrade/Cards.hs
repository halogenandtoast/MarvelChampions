module Marvel.Upgrade.Cards where

import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.Trait qualified as MGTrait
import Marvel.Resource qualified as MGResource
import Marvel.Boost qualified as MGBoost
import Marvel.Keyword qualified as SpdrKeyword
import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait
import Marvel.Keyword qualified as SMKeyword
import Marvel.GameValue.Types qualified as SMValue
import Marvel.Boost qualified as SMBoost

import Marvel.Ability.Types qualified as Ability

import Marvel.EncounterSet
import Marvel.Cost.Types
import Marvel.Window
import Marvel.Prelude

import Marvel.GameValue.Types (GameValue (PerPlayer))

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Data.HashSet (singleton)
import Marvel.Criteria
import Marvel.EncounterSet (EncounterSet (MadTitansShadowCampaign, TheMarket))
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Name
import Marvel.Resource
import Marvel.Trait

allUpgrades :: HashMap CardCode CardDef
allUpgrades = fromList $ concatMap
  toCardCodePairs
  [ stormsCrown, stormsCape, leadershipSkill
  , wolverinesClaws, adamantiumSkeleton, berserkerFrenzy, iGotBetter, warriorSkill
  , rubyQuartzVisor, fieldCommander, exploitWeakness, practicedDefense, priorityTarget, dangerRoomTraining, coordinatedAttack, marked, befuddle, pinnedDown, honoraryXMen
  , mgCoupDeGrace, mgSwagger, mgBrazenDefense, mgFerociousAttack, mgWarCry, mgCommanderCoupDeGrace, mgCompassion, mgGroupAssault, mgShockAndAwe, mgImprovisation, mgDefenderSwagger, mgSurprise, mgHeroicIntervention, mgDeterminedDefense, mgBodyguard, mgPeacekeeperSurprise, mgPeacekeeperCompassion, mgRescueOperation, mgMentorship, mgFortitude
  , smCompactDartsA, smCompactDartsB, smImpactDampeningSuitA, smImpactDampeningSuitB, smLaserGogglesA, smLaserGogglesB, smPropulsionGauntletA, smPropulsionGauntletB, smRetinalDisplayA, smRetinalDisplayB, smShockKnucklesA, smShockKnucklesB, smWaveBracersA, smWaveBracersB, smWristNavigatorA, smWristNavigatorB
  , spiderTracer
  , webShooter
  , webbedUp
  , captainMarvelsHelmet
  , cosmicFlight
  , energyChannel
  , focusedRage
  , superhumanStrength
  , arcReactor
  , markVArmor
  , markVHelmet
  , poweredGauntlets
  , rocketBoots
  , energyDaggers
  , pantherClaws
  , tacticalGenius
  , vibraniumSuit
  , combatTraining
  , heroicIntuition
  , inspired
  , armoredVest
  , indomitable
  , tenacity
  , captainAmericasHelmet
  , captainAmericasShield
  , superSoldierSerum
  , honoraryAvenger
  , enraged
  , followed
  , enhancedAwareness
  , biokineticPolymerSuit
  , embiggen
  , shrink
  , energyBarrier
  , endurance
  , enhancedReflexes
  , downTime
  , attacrobatics
  , blackWidowsGauntlet
  , grapplingHook
  , synthSuit
  , widowsBite
  , counterintelligence
  , spycraft
  , targetAcquired
  , counterattack
  , rapidResponse
  , defensiveStance
  , espionage
  , godOfThunder
  , mjolnir
  , thorsHelmet
  , battleFury
  , jarnbjorn
  , underSurveillance
  , enhancedPhysique
  , cloakOfLevitation
  , magicalEnhancements
  , theEyeOfAgamotto
  , unflappable
  , theSorcererSupreme
  , boundlessRage
  , immovableObject
  , martialProwess
  , electrostaticArmor
  , resourceful
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , hawkeyesBow
  , hawkeyesQuiver
  , expertMarksman
  , skyCycle
  , finesse
  , skilledInvestigator

  -- The Rise of Red Skull: the Hydra Campaign set
  , adrenalStims
  , tacticalScanner
  , emergencyTeleporter
  , laserCannon
  , basicThwartUpgrade
  , improvedThwartUpgrade
  , basicAttackUpgrade
  , improvedAttackUpgrade
  , basicDefenseUpgrade
  , improvedDefenseUpgrade
  , basicRecoveryUpgrade
  , improvedRecoveryUpgrade
  -- The Once and Future Kang


  -- Ant-Man
  , antMansHelmet
  , giantStrength
  , wristGauntlets
  , powerGloves
  , reinforcedSuit

  -- Wasp
  , redRoomTraining
  , bioSyntheticWings
  , waspsHelmet
  , lieInWait

  -- Quicksilver
  , acceleratedReflex
  , frictionResistance
  , hyperPerception
  , reinforcedSinew
  , nervesOfSteel
  , adrenalineRush
  , civicDuty
  , bruteForce
  , senseOfJustice

  -- Scarlet Witch
  , magicShield
  , scarletWitchsCrest

  -- Star-Lord
  , elementGun
  , jetBoots
  , leaderOfTheGuardians
  , starLordsHelmet
  , laserBlaster
  , pulseGrenade

  -- Gamora
  , keenInstincts
  , gamorasSword
  , godslayer
  , commsImplant

  -- Groot
  , entanglingVines
  , lashingVines
  , vineShield
  , vineSpikes
  , dauntless
  , hardToIgnore
  , deftFocus

  -- Rocket Raccoon
  , batteryPack
  , cyberneticSkeleton
  , particleCannon
  , rocketLauncher
  , rocketsPistol
  , thrusterBoots
  , followThrough
  , handCannon
  , boosterBoots

  -- The Galaxy's Most Wanted: The Market
  , armorPlating
  , heavyCannon
  , hyperThrusters
  , reactorCore
  , cargoHold
  , mountedLaser
  , navigationColumn
  , targetingScreen
  -- Drax
  , draxsKnife
  , draxsOtherKnife
  , dwiTheetMastery
  , tooStubbornToDie

  -- Venom
  , multiGun
  , spiderSense
  , venomsPistol
  , sonicRifle
  , sideHolster
  , plasmaPistol

  -- Spectrum
  , gamma
  , photon
  , pulsar
  , energyDuplication
  , readyToRumble

  -- Adam Warlock
  , karmicStaff
  , warlocksCape
  , cosmicWard
  , mysticSenses

  -- The Mad Titan's Shadow: the campaign set
  , nornStoneSetup
  , nornStone
  , cutthroatAmbition
  , evasiveManeuvering
  , unyieldingPersistence
  , weaponsMaster
  , wideStance
  , justiceServed
  , energySpear
  , honoraryGuardian
  , upgradedChassis
  , gauntletGun
  , missileLauncher
  , shoulderCannon
  , sidearm
  , deathGlow
  , valkyriesSpear
  , dragonfang
  , aragorn
  , flightOfTheValkyrior
  , anticipation
  , intangible
  , solarGem
  , visionsCape
  , densityControl
  , flowLikeWater
  , chanceEncounter
  , dense
  , ticketToTheMultiverse
  , webBracelet
  , planB
  , powerWithin
  , defenseMechanism
  , webShooterMilesMorales
  , symbioteSuit
  , supernovaHelmet
  , fluidMotion
  , honedTechnique
  , heightAdvantage
  , photonBlasters
  , propulsionJets
  , ingenuity
  , cartoonPhysics
  , hugeWoodenHammer
  , organicWebbing
  , overwatch
  , warriorOfTheGreatWeb
  , spdrPilot
  , hostSpider
  , psychicLink
  , speedMetalAlloy
  , webFluidCompressor
  , forcefieldGenerator
  , spiderTingle
  , unshakable
  , clarityOfPurpose
  , ironWill
  , titaniumMuscles
  , organicSteel
  , protectiveTraining
  , solid
  , phased
  , acuteControl
  , intangibleInterference
  , phasedAndConfused
  , attackTraining
  , gatekeeper
  , phoenixForceRestrained
  , phoenixForceUnleashed
  , phoenixSuit
  , riseFromTheAshes
  , telekineticShield
  , mentalParalysis
  , mindControl
  , missionTraining
  ]

upgrade
  :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> Aspect -> CardDef
upgrade code name cost traits resources aspect =
  baseUpgrade code name cost traits resources (Just aspect)

identityUpgrade :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
identityUpgrade code name cost traits resources =
  baseUpgrade code name cost traits resources Nothing

basicUpgrade :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
basicUpgrade code name cost traits resources =
  baseUpgrade code name cost traits resources (Just Neutral)

baseUpgrade
  :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> Maybe Aspect -> CardDef
baseUpgrade code name cost traits resources mAspect = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Just cost
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = UpgradeType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = map (PrintedResource, ) resources
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

unique :: CardDef -> CardDef
unique def = def { cdUnique = True }

limited :: InPlayLimit -> CardDef -> CardDef
limited limit def = def { cdLimit = Just limit }

spiderTracer :: CardDef
spiderTracer = (identityUpgrade "01007" "Spider-Tracer" 1 [Item, Tech] [Energy]
               )
  { cdCriteria = MinionExists AnyMinion
  }

webShooter :: CardDef
webShooter = identityUpgrade "01008" "Web-Shooter" 1 [Item, Tech] [Physical]

webbedUp :: CardDef
webbedUp = (identityUpgrade "01009" "Webbed Up" 4 [Condition] [Physical])
  { cdCriteria = InHeroForm
  , cdLimit = Just (MaxPerEnemy 1)
  }

captainMarvelsHelmet :: CardDef
captainMarvelsHelmet = unique $ identityUpgrade
  "01016"
  "Captain Marvel's Helmet"
  2
  [Armor, Tech]
  [Physical]

cosmicFlight :: CardDef
cosmicFlight = identityUpgrade "01017" "Cosmic Flight" 2 [Superpower] [Energy]

energyChannel :: CardDef
energyChannel = limited (MaxPerPlayer 1)
  $ identityUpgrade "01018" "Energy Channel" 0 [Superpower] [Mental]

focusedRage :: CardDef
focusedRage = identityUpgrade "01027" "Focused Rage" 3 [Skill] [Energy]

superhumanStrength :: CardDef
superhumanStrength =
  identityUpgrade "01028" "Superhuman Strength" 2 [Superpower] [Mental]

arcReactor :: CardDef
arcReactor =
  unique $ identityUpgrade "01035" "Arc Reactor" 2 [Item, Tech] [Energy]

markVArmor :: CardDef
markVArmor =
  unique $ identityUpgrade "01036" "Mark V Armor" 3 [Armor, Tech] [Mental]

markVHelmet :: CardDef
markVHelmet =
  unique $ identityUpgrade "01037" "Mark V Helmet" 1 [Armor, Tech] [Physical]

poweredGauntlets :: CardDef
poweredGauntlets =
  identityUpgrade "01038" "Powered Gauntlets" 2 [Armor, Tech] [Energy]

rocketBoots :: CardDef
rocketBoots = identityUpgrade "01039" "Rocket Boots" 1 [Armor, Tech] [Mental]

energyDaggers :: CardDef
energyDaggers =
  identityUpgrade "01046" "Energy Daggers" 2 [BlackPanther, Weapon] [Mental]

pantherClaws :: CardDef
pantherClaws =
  identityUpgrade "01047" "Panther Claws" 2 [BlackPanther, Weapon] [Energy]

tacticalGenius :: CardDef
tacticalGenius =
  identityUpgrade "01048" "Tactical Genius" 2 [BlackPanther, Skill] [Physical]

vibraniumSuit :: CardDef
vibraniumSuit =
  identityUpgrade "01049" "Vibranium Suit" 2 [Armor, BlackPanther] [Mental]

combatTraining :: CardDef
combatTraining = deckLimited 3 $ reprintedAs ["04041", "18017", "21045", "25017"] $ limited (MaxPerPlayerUnderAnyControl 1)
  $ upgrade "01057" "Combat Training" 2 [Skill] [Physical] Aggression

heroicIntuition :: CardDef
heroicIntuition = deckLimited 3 $ reprintedAs ["04046", "15017", "21051", "22019"] $ limited (MaxPerPlayerUnderAnyControl 1)
  $ upgrade "01065" "Heroic Intuition" 2 [Skill] [Energy] Justice

inspired :: CardDef
inspired = reprintedAs ["21057"] $ (upgrade "01074" "Inspired" 1 [Condition] [Physical] Leadership)
  { cdCriteria = AllyExists AnyAlly
  , cdLimit = Just (MaxPerAlly 1)
  }

armoredVest :: CardDef
armoredVest = reprintedAs ["14016", "21063"] $ limited (MaxPerPlayerUnderAnyControl 1)
  $ upgrade "01081" "Armored Vest" 1 [Armor] [Mental] Protection

indomitable :: CardDef
indomitable = deckLimited 3 $ reprintedAs ["16018", "19019", "26017"] $ upgrade "01082" "Indomitable" 1 [Condition] [Energy] Protection

tenacity :: CardDef
tenacity = basicUpgrade "01093" "Tenacity" 2 [Condition] [Energy]

captainAmericasHelmet :: CardDef
captainAmericasHelmet =
  unique $ identityUpgrade "03008" "Captain America's Helmet" 1 [Armor] [Physical]

captainAmericasShield :: CardDef
captainAmericasShield =
  (unique $ identityUpgrade "03009" "Captain America's Shield" 1 [Item] [Wild])
    { cdKeywords = singleton Restricted
    }

superSoldierSerum :: CardDef
superSoldierSerum =
  identityUpgrade "03010" "Super-Soldier Serum" 2 [Item] [Physical]

honoraryAvenger :: CardDef
honoraryAvenger =
  (basicUpgrade "03025" "Honorary Avenger" 0 [Title] [Physical])
    { cdCriteria = SelfMatches (IdentityWithTrait Avenger)
    , cdLimit = Just (MaxPerCharacter 1)
    }

enraged :: CardDef
enraged = limited (MaxPerAlly 1) $
  upgrade "03031" "Enraged" 1 [Condition] [Physical] Aggression

followed :: CardDef
followed = reprintedAs ["30018"] $ limited (MaxPerSideScheme 1) $
  upgrade "03032" "Followed" 1 [] [Energy] Justice

enhancedAwareness :: CardDef
enhancedAwareness = reprintedAs ["17031"] $
  basicUpgrade "03034" "Enhanced Awareness" 2 [Condition] [Mental]

biokineticPolymerSuit :: CardDef
biokineticPolymerSuit =
  unique $ identityUpgrade "05009" "Biokinetic Polymer Suit" 1 [Armor] [Mental]

embiggen :: CardDef
embiggen = identityUpgrade "05010" "Embiggen!" 2 [Condition, Superpower] [Energy]

shrink :: CardDef
shrink = identityUpgrade "05011" "Shrink" 2 [Condition, Superpower] [Mental]

energyBarrier :: CardDef
energyBarrier = reprintedAs ["31018"] $ upgrade "05017" "Energy Barrier" 2 [Item, Tech] [Energy] Protection

endurance :: CardDef
endurance = reprintedAs ["36026"] $ limited (MaxPerPlayerUnderAnyControl 1)
  $ basicUpgrade "05023" "Endurance" 1 [Condition] [Energy]

enhancedReflexes :: CardDef
enhancedReflexes = reprintedAs ["18032"] $ basicUpgrade "05024" "Enhanced Reflexes" 2 [Condition] [Energy]

downTime :: CardDef
downTime = reprintedAs ["34024"] $ limited (MaxPerPlayerUnderAnyControl 1)
  $ basicUpgrade "05033" "Down Time" 1 [Condition] [Physical]
boundlessRage :: CardDef
boundlessRage =
  (identityUpgrade "10009" "Boundless Rage" 1 [Condition, Gamma] [Physical])
    { cdCriteria = InHeroForm
    }

immovableObject :: CardDef
immovableObject = identityUpgrade "10010" "Immovable Object" 3 [Condition] [Physical]

martialProwess :: CardDef
martialProwess = limited (MaxPerPlayerUnderAnyControl 1)
  $ upgrade "10018" "Martial Prowess" 2 [Skill] [Physical] Aggression

electrostaticArmor :: CardDef
electrostaticArmor = limited (MaxPerPlayerUnderAnyControl 1)
  $ upgrade "10031" "Electrostatic Armor" 1 [Armor, Tech] [Energy] Protection

resourceful :: CardDef
resourceful = reprintedAs ["20020"] $ basicUpgrade "10032" "Resourceful" 1 [Skill] [Wild]

-- | "Play only if you control a Spy character."
youControlASpy :: Criteria
youControlASpy = CharacterExists (CharacterControlledBy You <> CharacterWithTrait Spy)

attacrobatics :: CardDef
attacrobatics =
  identityUpgrade "08006" "Attacrobatics" 1 [Attack, Preparation, Skill] [Physical]

blackWidowsGauntlet :: CardDef
blackWidowsGauntlet =
  identityUpgrade "08007" "Black Widow's Gauntlet" 1 [Tech] [Physical]

grapplingHook :: CardDef
grapplingHook =
  identityUpgrade "08008" "Grappling Hook" 2 [Preparation, Tech] [Mental]

synthSuit :: CardDef
synthSuit = unique $ identityUpgrade "08009" "Synth-Suit" 3 [Armor, Tech] [Energy]

widowsBite :: CardDef
widowsBite = identityUpgrade "08010" "Widow's Bite" 1 [Preparation, Tech] [Energy]

counterintelligence :: CardDef
counterintelligence = limited (MaxPerPlayer 1)
  $ upgrade "08017" "Counterintelligence" 2 [Preparation, Skill] [Energy] Justice

spycraft :: CardDef
spycraft = (upgrade "08018" "Spycraft" 1 [Preparation, Skill] [Physical] Justice)
  { cdCriteria = youControlASpy
  }

targetAcquired :: CardDef
targetAcquired = limited (MaxPerPlayer 1)
  $ basicUpgrade "08024" "Target Acquired" 1 [Preparation, Skill] [Physical]

counterattack :: CardDef
counterattack = limited (MaxPerPlayer 1)
  $ upgrade "08030" "Counterattack" 1 [Preparation, Tactic] [Physical] Aggression

rapidResponse :: CardDef
rapidResponse = limited (MaxPerPlayer 1)
  $ upgrade "08031" "Rapid Response" 2 [Preparation, Tactic] [Mental] Leadership

defensiveStance :: CardDef
defensiveStance = limited (MaxPerPlayer 1)
  $ upgrade "08032" "Defensive Stance" 2 [Preparation, Tactic] [Energy] Protection

espionage :: CardDef
espionage = (basicUpgrade "08033" "Espionage" 1 [Preparation, Skill] [Physical])
  { cdCriteria = youControlASpy
  }

godOfThunder :: CardDef
godOfThunder = identityUpgrade "06008" "God of Thunder" 2 [Asgard, Title] [Energy]

mjolnir :: CardDef
mjolnir =
  unique $
    (identityUpgrade "06009" "Mjolnir" 1 [Asgard, Weapon] [Energy])
      { cdKeywords = fromList [Restricted]
      }

thorsHelmet :: CardDef
thorsHelmet = unique $ identityUpgrade "06010" "Thor's Helmet" 2 [Asgard, Armor] [Mental]

battleFury :: CardDef
battleFury = reprintedAs ["35015"] $ limited (MaxPerPlayerUnderAnyControl 1)
  $ upgrade "06018" "Battle Fury" 1 [Condition] [Energy] Aggression

jarnbjorn :: CardDef
jarnbjorn =
  unique $
    (upgrade "06019" "Jarnbjorn" 1 [Asgard, Weapon] [Physical] Aggression)
      { cdKeywords = fromList [Restricted]
      }

underSurveillance :: CardDef
underSurveillance = limited (MaxPerScheme 1)
  $ upgrade "06031" "Under Surveillance" 2 [Condition] [Energy] Justice

enhancedPhysique :: CardDef
enhancedPhysique = reprintedAs ["19033"] $ basicUpgrade "06034" "Enhanced Physique" 2 [Condition] [Physical]

cloakOfLevitation :: CardDef
cloakOfLevitation =
  unique $ identityUpgrade "09009" "Cloak of Levitation" 2 [Artifact, Item] [Physical]

magicalEnhancements :: CardDef
magicalEnhancements = limited UnderAnyControl
  $ identityUpgrade "09010" "Magical Enhancements" 1 [Condition, Spell] [Energy]

theEyeOfAgamotto :: CardDef
theEyeOfAgamotto =
  unique $ identityUpgrade "09011" "The Eye of Agamotto" 2 [Artifact, Item] [Energy]

unflappable :: CardDef
unflappable = limited (MaxPerPlayerUnderAnyControl 1)
  $ upgrade "09020" "Unflappable" 1 [Condition] [Mental] Protection

theSorcererSupreme :: CardDef
theSorcererSupreme =
  (unique $ basicUpgrade "09026" "The Sorcerer Supreme" 2 [Title] [Mental])
    { cdCriteria = SelfMatches (IdentityWithTrait Mystic)
    }

-- * The Rise of Red Skull: the Hydra Campaign set, campaign-only cards

-- | A Tech upgrade: Setup, its own wild resource, and nothing to pay.
techUpgrade :: CardCode -> Name -> CardDef
techUpgrade code name = (basicUpgrade code name 0 [Tech] [Wild]) {cdKeywords = singleton Setup}

-- | A Condition upgrade's face: Permanent and Setup, with no cost.
conditionUpgrade :: CardCode -> Name -> CardDef
conditionUpgrade code name = (basicUpgrade code name 0 [Condition] []) {cdCost = Nothing, cdKeywords = fromList [Permanent, Setup]}

adrenalStims :: CardDef
adrenalStims = techUpgrade "04155" "Adrenal Stims"

tacticalScanner :: CardDef
tacticalScanner = techUpgrade "04156" "Tactical Scanner"

emergencyTeleporter :: CardDef
emergencyTeleporter = techUpgrade "04157" "Emergency Teleporter"

laserCannon :: CardDef
laserCannon = techUpgrade "04158" "Laser Cannon"

basicThwartUpgrade :: CardDef
basicThwartUpgrade = conditionUpgrade "04159a" "Basic Thwart Upgrade"

improvedThwartUpgrade :: CardDef
improvedThwartUpgrade = conditionUpgrade "04159b" "Improved Thwart Upgrade"

basicAttackUpgrade :: CardDef
basicAttackUpgrade = conditionUpgrade "04160a" "Basic Attack Upgrade"

improvedAttackUpgrade :: CardDef
improvedAttackUpgrade = conditionUpgrade "04160b" "Improved Attack Upgrade"

basicDefenseUpgrade :: CardDef
basicDefenseUpgrade = conditionUpgrade "04161a" "Basic Defense Upgrade"

improvedDefenseUpgrade :: CardDef
improvedDefenseUpgrade = conditionUpgrade "04161b" "Improved Defense Upgrade"

basicRecoveryUpgrade :: CardDef
basicRecoveryUpgrade = conditionUpgrade "04162a" "Basic Recovery Upgrade"

improvedRecoveryUpgrade :: CardDef
improvedRecoveryUpgrade = conditionUpgrade "04162b" "Improved Recovery Upgrade"
hawkeyesBow :: CardDef
hawkeyesBow =
  unique $
    (identityUpgrade "04002" "Hawkeye's Bow" 0 [Weapon] [Wild])
      { cdKeywords = fromList [Restricted]
      }

hawkeyesQuiver :: CardDef
hawkeyesQuiver = unique $ identityUpgrade "04003" "Hawkeye's Quiver" 1 [Item] [Mental]

expertMarksman :: CardDef
expertMarksman = identityUpgrade "04010" "Expert Marksman" 1 [Skill] [Wild]

skyCycle :: CardDef
skyCycle = limited (MaxPerAlly 1) $ upgrade "04015" "Sky Cycle" 1 [Vehicle] [Energy] Leadership

finesse :: CardDef
finesse = identityUpgrade "04033" "Finesse" 2 [Condition] [Mental]

skilledInvestigator :: CardDef
skilledInvestigator =
  limited (MaxPerPlayerUnderAnyControl 1) $
    upgrade "04047" "Skilled Investigator" 0 [Skill] [Physical] Justice

-- Ant-Man

antMansHelmet :: CardDef
antMansHelmet = unique $ identityUpgrade "12008" "Ant-Man's Helmet" 3 [Armor, Tech] [Mental]

giantStrength :: CardDef
giantStrength = identityUpgrade "12009" "Giant Strength" 1 [Condition, Giant] [Energy]

wristGauntlets :: CardDef
wristGauntlets = identityUpgrade "12010" "Wrist Gauntlets" 1 [Item, Tech] [Mental]

powerGloves :: CardDef
powerGloves = upgrade "12017" "Power Gloves" 1 [Armor, Tech] [Mental] Leadership

reinforcedSuit :: CardDef
reinforcedSuit = upgrade "12018" "Reinforced Suit" 1 [Armor, Tech] [Energy] Leadership

-- Wasp

redRoomTraining :: CardDef
redRoomTraining = identityUpgrade "13008" "Red Room Training" 2 [Skill] [Physical]

bioSyntheticWings :: CardDef
bioSyntheticWings = unique $ identityUpgrade "13009" "Bio-Synthetic Wings" 2 [Item, Tech] [Energy]

waspsHelmet :: CardDef
waspsHelmet = unique $ identityUpgrade "13010" "Wasp's Helmet" 2 [Armor, Tech] [Physical]

lieInWait :: CardDef
lieInWait = limited (MaxPerPlayer 1) $ upgrade "13017" "Lie in Wait" 1 [Preparation] [Energy] Aggression

-- Quicksilver

acceleratedReflex :: CardDef
acceleratedReflex = identityUpgrade "14008" "Accelerated Reflex" 2 [Condition, Superpower] [Energy]

frictionResistance :: CardDef
frictionResistance = identityUpgrade "14009" "Friction Resistance" 3 [Condition, Superpower] [Physical]

hyperPerception :: CardDef
hyperPerception = identityUpgrade "14010" "Hyper Perception" 2 [Condition, Superpower] [Mental]

reinforcedSinew :: CardDef
reinforcedSinew = identityUpgrade "14011" "Reinforced Sinew" 2 [Condition, Superpower] [Physical]

nervesOfSteel :: CardDef
nervesOfSteel = limited (MaxPerPlayerUnderAnyControl 1) $ upgrade "14017" "Nerves of Steel" 2 [Skill] [Energy] Protection

adrenalineRush :: CardDef
adrenalineRush = basicUpgrade "14022" "Adrenaline Rush" 1 [Condition] [Energy]

civicDuty :: CardDef
civicDuty = basicUpgrade "14023" "Civic Duty" 1 [Tactic] [Mental]

bruteForce :: CardDef
bruteForce = upgrade "14029" "Brute Force" 1 [Skill] [Mental] Aggression

senseOfJustice :: CardDef
senseOfJustice = limited (MaxPerPlayerUnderAnyControl 1) $ upgrade "14030" "Sense of Justice" 2 [Skill] [Mental] Justice

-- Scarlet Witch

magicShield :: CardDef
magicShield = identityUpgrade "15008" "Magic Shield" 1 [Spell] [Physical]

scarletWitchsCrest :: CardDef
scarletWitchsCrest = identityUpgrade "15009" "Scarlet Witch's Crest" 2 [Item] [Energy]

-- Star-Lord

elementGun :: CardDef
elementGun =
  (identityUpgrade "17007" "Element Gun" 3 [Tech, Weapon] [Energy])
    { cdKeywords = singleton Restricted
    }

jetBoots :: CardDef
jetBoots = identityUpgrade "17008" "Jet Boots" 2 [Item, Tech] [Mental]

leaderOfTheGuardians :: CardDef
leaderOfTheGuardians = unique $ identityUpgrade "17009" "Leader of the Guardians" 3 [Title] [Mental]

starLordsHelmet :: CardDef
starLordsHelmet = unique $ identityUpgrade "17010" "Star-Lord's Helmet" 1 [Armor, Tech] [Mental]

-- | Attach to a Guardian ally. Max 1 per ally.
laserBlaster :: CardDef
laserBlaster =
  (upgrade "17019" "Laser Blaster" 1 [Tech, Weapon] [Physical] Leadership)
    { cdCriteria = AllyExists (AllyWithTrait Guardian)
    , cdLimit = Just (MaxPerAlly 1)
    }

pulseGrenade :: CardDef
pulseGrenade = basicUpgrade "17023" "Pulse Grenade" 2 [Weapon] [Energy]

-- Gamora

keenInstincts :: CardDef
keenInstincts = identityUpgrade "18009" "Keen Instincts" 1 [Skill] [Energy]

gamorasSword :: CardDef
gamorasSword =
  (unique $ identityUpgrade "18010" "Gamora's Sword" 3 [Weapon] [Physical])
    { cdKeywords = singleton Restricted
    }

godslayer :: CardDef
godslayer =
  (unique $ upgrade "18018" "Godslayer" 3 [Weapon] [Mental] Aggression)
    { cdKeywords = singleton Restricted
    }

-- | Attach to a Guardian ally. Max 1 per ally.
commsImplant :: CardDef
commsImplant =
  (upgrade "18030" "Comms Implant" 1 [Item, Tech] [Mental] Leadership)
    { cdCriteria = AllyExists (AllyWithTrait Guardian)
    , cdLimit = Just (MaxPerAlly 1)
    }

-- Groot

entanglingVines :: CardDef
entanglingVines = identityUpgrade "16008" "Entangling Vines" 1 [Superpower] [Mental]

lashingVines :: CardDef
lashingVines = identityUpgrade "16009" "Lashing Vines" 1 [Superpower] [Energy]

vineShield :: CardDef
vineShield = identityUpgrade "16010" "Vine Shield" 1 [Superpower] [Physical]

vineSpikes :: CardDef
vineSpikes = identityUpgrade "16011" "Vine Spikes" 1 [Superpower] [Physical]

dauntless :: CardDef
dauntless = limited (MaxPerPlayerUnderAnyControl 1) $ upgrade "16016" "Dauntless" 1 [Condition] [Energy] Protection

hardToIgnore :: CardDef
hardToIgnore = upgrade "16017" "Hard to Ignore" 1 [Skill] [Physical] Protection

deftFocus :: CardDef
deftFocus = limited (MaxPerPlayer 1) $ basicUpgrade "16024" "Deft Focus" 1 [Skill] [Energy]

-- Rocket Raccoon

batteryPack :: CardDef
batteryPack = identityUpgrade "16034" "Battery Pack" 1 [Item, Tech] [Energy]

cyberneticSkeleton :: CardDef
cyberneticSkeleton = identityUpgrade "16035" "Cybernetic Skeleton" 2 [Tech] [Energy]

particleCannon :: CardDef
particleCannon =
  (identityUpgrade "16036" "Particle Cannon" 3 [Tech, Weapon] [Mental])
    { cdKeywords = singleton Restricted
    }

rocketLauncher :: CardDef
rocketLauncher =
  (identityUpgrade "16037" "Rocket Launcher" 3 [Tech, Weapon] [Physical])
    { cdKeywords = singleton Restricted
    }

rocketsPistol :: CardDef
rocketsPistol =
  (identityUpgrade "16038" "Rocket's Pistol" 1 [Tech, Weapon] [Energy])
    { cdKeywords = singleton Restricted
    }

thrusterBoots :: CardDef
thrusterBoots = identityUpgrade "16039" "Thruster Boots" 1 [Armor, Tech] [Mental]

followThrough :: CardDef
followThrough = upgrade "16045" "Follow Through" 2 [Skill] [Mental] Aggression

-- | Its uses (3 charge counters) are its attrs'.
handCannon :: CardDef
handCannon =
  (upgrade "16046" "Hand Cannon" 2 [Tech, Weapon] [Physical] Aggression)
    { cdKeywords = singleton Restricted
    }

-- | "Play only if your identity has the Guardian trait. Max 1 per player."
boosterBoots :: CardDef
boosterBoots =
  limited (MaxPerPlayer 1) $
    (basicUpgrade "16052" "Booster Boots" 1 [Armor, Tech] [Mental])
      { cdCriteria = SelfMatches (IdentityWithTrait Guardian)
      }

-- The Galaxy's Most Wanted: The Market (unit costs in Marvel.Campaign.marketUnitCosts)

armorPlating :: CardDef
armorPlating = marketUpgrade "16162" "Armor Plating" 0 [Armor, MilanoMod]

heavyCannon :: CardDef
heavyCannon = marketUpgrade "16163" "Heavy Cannon" 0 [MilanoMod, Weapon]

hyperThrusters :: CardDef
hyperThrusters = marketUpgrade "16164" "Hyper Thrusters" 1 [MilanoMod, Tech]

reactorCore :: CardDef
reactorCore = marketUpgrade "16165" "Reactor Core" 1 [MilanoMod, Tech]

cargoHold :: CardDef
cargoHold = marketUpgrade "16170" "Cargo Hold" 0 [Location, MilanoMod]

mountedLaser :: CardDef
mountedLaser = marketUpgrade "16171" "Mounted Laser" 1 [MilanoMod, Weapon]

navigationColumn :: CardDef
navigationColumn = marketUpgrade "16172" "Navigation Column" 2 [MilanoMod, Tech]

targetingScreen :: CardDef
targetingScreen = marketUpgrade "16173" "Targeting Screen" 2 [MilanoMod, Tech]

-- | A card of The Market, as 'Marvel.Event.Cards.marketEvent'.
marketUpgrade :: CardCode -> Name -> Int -> [Trait] -> CardDef
marketUpgrade code name cost traits =
  (baseUpgrade code name cost traits [Wild] Nothing) {cdEncounterSet = Just TheMarket}
-- Drax

draxsKnife :: CardDef
draxsKnife =
  (unique $ identityUpgrade "19008" "Drax's Knife" 1 [Weapon] [Wild])
    { cdKeywords = singleton Restricted
    }

draxsOtherKnife :: CardDef
draxsOtherKnife =
  (unique $ identityUpgrade "19009" "Drax's Other Knife" 1 [Weapon] [Wild])
    { cdKeywords = singleton Restricted
    }

dwiTheetMastery :: CardDef
dwiTheetMastery = identityUpgrade "19010" "DWI Theet Mastery" 1 [Skill] [Mental]

tooStubbornToDie :: CardDef
tooStubbornToDie = identityUpgrade "19011" "Too Stubborn to Die" 2 [Condition] [Mental]

-- Venom

multiGun :: CardDef
multiGun =
  (unique $ identityUpgrade "20008" "Multi-Gun" 3 [Tech, Weapon] [Wild])
    { cdKeywords = singleton Restricted
    }

spiderSense :: CardDef
spiderSense = identityUpgrade "20009" "Spider-Sense" 2 [Superpower] [Mental]

venomsPistol :: CardDef
venomsPistol =
  (identityUpgrade "20010" "Venom's Pistol" 1 [Tech, Weapon] [Energy])
    { cdKeywords = singleton Restricted
    }

sonicRifle :: CardDef
sonicRifle =
  (upgrade "20015" "Sonic Rifle" 3 [Tech, Weapon] [Mental] Justice)
    { cdKeywords = singleton Restricted
    }

sideHolster :: CardDef
sideHolster = limited (MaxPerPlayerUnderAnyControl 1) $ basicUpgrade "20021" "Side Holster" 0 [Item] [Physical]

plasmaPistol :: CardDef
plasmaPistol =
  (basicUpgrade "20022" "Plasma Pistol" 2 [Tech, Weapon] [Energy])
    { cdKeywords = singleton Restricted
    }

-- Spectrum

-- | An energy form: Permanent, set aside before setup and put into play facedown by
-- Monica Rambeau's Setup; faceup, it is the form Spectrum is in.
energyForm :: CardCode -> Name -> Resource -> CardDef
energyForm code name r =
  (identityUpgrade code name 0 [] [r]) {cdCost = Nothing, cdKeywords = fromList [EnergyForm, Permanent]}

gamma :: CardDef
gamma = energyForm "21002" "Gamma" Physical

photon :: CardDef
photon = energyForm "21003" "Photon" Mental

pulsar :: CardDef
pulsar = energyForm "21004" "Pulsar" Energy

energyDuplication :: CardDef
energyDuplication = identityUpgrade "21006" "Energy Duplication" 2 [Superpower] [Wild]

readyToRumble :: CardDef
readyToRumble = reprintedAs ["32051"] $ limited (MaxPerPlayerUnderAnyControl 1) $ basicUpgrade "21022" "Ready to Rumble" 1 [Condition] [Energy]

-- Adam Warlock

karmicStaff :: CardDef
karmicStaff = unique $ identityUpgrade "21034" "Karmic Staff" 2 [Artifact, Item] [Energy]

warlocksCape :: CardDef
warlocksCape = unique $ identityUpgrade "21035" "Warlock's Cape" 2 [Item] [Physical]

cosmicWard :: CardDef
cosmicWard = identityUpgrade "21036" "Cosmic Ward" 1 [Spell] [Energy]

mysticSenses :: CardDef
mysticSenses = identityUpgrade "21037" "Mystic Senses" 2 [Condition, Superpower] [Mental]

-- The Mad Titan's Shadow: the campaign set

-- | Its Setup side: Permanent and Setup, flipping to its other side.
nornStoneSetup :: CardDef
nornStoneSetup = (nornStoneFace "21187a") {cdKeywords = fromList [Permanent, Setup]}

nornStone :: CardDef
nornStone = nornStoneFace "21187b"

nornStoneFace :: CardCode -> CardDef
nornStoneFace code =
  (basicUpgrade code "Norn Stone" 0 [Asgard, Artifact] [])
    { cdKeywords = singleton Permanent
    , cdEncounterSet = Just MadTitansShadowCampaign
    , cdEncounterSetQuantity = Just 4
    }

-- Nebula

cutthroatAmbition :: CardDef
cutthroatAmbition = deckLimited 2 $ (identityUpgrade "22004" "Cutthroat Ambition" 1 [Technique] [Mental]) {cdAbilitySubTypes = [Ability.Thwart]}

evasiveManeuvering :: CardDef
evasiveManeuvering = deckLimited 1 $ identityUpgrade "22005" "Evasive Maneuvering" 1 [Technique] [Energy]

unyieldingPersistence :: CardDef
unyieldingPersistence = deckLimited 1 $ identityUpgrade "22006" "Unyielding Persistence" 1 [Technique] [Mental]

weaponsMaster :: CardDef
weaponsMaster = deckLimited 2 $ (identityUpgrade "22007" "Weapons Master" 1 [Technique] [Physical]) {cdAbilitySubTypes = [Ability.Attack]}

wideStance :: CardDef
wideStance = deckLimited 2 $ identityUpgrade "22008" "Wide Stance" 1 [Technique] [Energy]

justiceServed :: CardDef
justiceServed = deckLimited 3 $ (upgrade "22014" "Justice Served" 1 [] [Mental] Justice)
    { cdLimit = Just (MaxPerPlayerUnderAnyControl 1)
    }

energySpear :: CardDef
energySpear = deckLimited 3 $ (upgrade "22032" "Energy Spear" 1 [Weapon] [Energy] Aggression)
    { cdLimit = Just (MaxPerAlly 1)
    }

honoraryGuardian :: CardDef
honoraryGuardian = deckLimited 3 $ (basicUpgrade "22035" "Honorary Guardian" 0 [Title] [Energy])
    { cdLimit = Just (MaxPerCharacter 1)
    , cdCriteria = SelfMatches (IdentityWithTrait Guardian)
    }

-- War Machine

upgradedChassis :: CardDef
upgradedChassis = deckLimited 1 $ identityUpgrade "23004" "Upgraded Chassis" 3 [Armor, Tech] [Mental]

gauntletGun :: CardDef
gauntletGun = deckLimited 2 $ identityUpgrade "23005" "Gauntlet Gun" 2 [Tech, Weapon] [Physical]

missileLauncher :: CardDef
missileLauncher = deckLimited 1 $ identityUpgrade "23006" "Missile Launcher" 1 [Tech, Weapon] [Mental]

shoulderCannon :: CardDef
shoulderCannon = deckLimited 1 $ identityUpgrade "23007" "Shoulder Cannon" 2 [Tech, Weapon] [Energy]

sidearm :: CardDef
sidearm = deckLimited 3 $ (basicUpgrade "23035" "Sidearm" 1 [Weapon] [Physical])
    { cdLimit = Just (MaxPerAlly 1)
    }

-- Valkyrie

deathGlow :: CardDef
deathGlow = deckLimited 1 $ identityUpgrade "25002" "Death-Glow" 1 [Condition] []

valkyriesSpear :: CardDef
valkyriesSpear = deckLimited 1 $ (identityUpgrade "25005" "Valkyrie's Spear" 1 [Asgard, Weapon] [Physical])
    { cdUnique = True
    , cdKeywords = fromList [Restricted]
    }

dragonfang :: CardDef
dragonfang = deckLimited 1 $ (identityUpgrade "25006" "Dragonfang" 1 [Asgard, Weapon] [Physical])
    { cdUnique = True
    , cdKeywords = fromList [Restricted]
    }

aragorn :: CardDef
aragorn = deckLimited 1 $ (identityUpgrade "25007" "Aragorn" 2 [Asgard, Creature] [Mental])
    { cdUnique = True
    }

flightOfTheValkyrior :: CardDef
flightOfTheValkyrior = deckLimited 2 $ identityUpgrade "25008" "Flight of the Valkyrior" 2 [] [Energy]

anticipation :: CardDef
anticipation = deckLimited 3 $ (upgrade "25035" "Anticipation" 1 [Preparation] [Physical] Protection)
    { cdLimit = Just (MaxPerPlayer 1)
    }

-- Vision

intangible :: CardDef
intangible = deckLimited 1 $ (identityUpgrade "26002" "Intangible" 0 [] [])
    { cdKeywords = fromList [Permanent, MassForm]
    }

solarGem :: CardDef
solarGem = deckLimited 1 $ (identityUpgrade "26005" "Solar Gem" 2 [Item] [Wild])
    { cdUnique = True
    }

visionsCape :: CardDef
visionsCape = deckLimited 1 $ (identityUpgrade "26006" "Vision's Cape" 2 [Item] [Physical])
    { cdUnique = True
    }

densityControl :: CardDef
densityControl = deckLimited 2 $ identityUpgrade "26007" "Density Control" 1 [Superpower] [Mental]

flowLikeWater :: CardDef
flowLikeWater = deckLimited 3 $ (upgrade "26016" "Flow Like Water" 2 [Condition] [Physical] Protection)
    { cdLimit = Just (MaxPerPlayerUnderAnyControl 1)
    }

chanceEncounter :: CardDef
chanceEncounter = deckLimited 3 $ (upgrade "26034" "Chance Encounter" 0 [Condition] [Energy] Justice)
    { cdLimit = Just (MaxPerSideScheme 1)
    }

-- The reverse of Intangible, flipped in place; its front keeps CDB code 26002.
dense :: CardDef
dense = intangible {cdCardCode = "26002b", cdName = "Dense", cdDeckLimit = Just 0}


-- Sinister Motives

ticketToTheMultiverse :: CardDef
ticketToTheMultiverse = (identityUpgrade "27008" "Ticket to the Multiverse" 3 [Item] [Wild, Wild])
    { cdDeckLimit = Just 1
    }

webBracelet :: CardDef
webBracelet = (identityUpgrade "27009" "Web-Bracelet" 2 [Item, Tech] [Physical])
    { cdDeckLimit = Just 2
    }

planB :: CardDef
planB = (basicUpgrade "27024" "Plan B" 1 [Tactic] [Physical])
    { cdDeckLimit = Just 3
    , cdLimit = Just (MaxPerPlayerUnderAnyControl 1)
    }

powerWithin :: CardDef
powerWithin = (identityUpgrade "27037" "Power Within" 1 [Superpower] [Energy])
    { cdDeckLimit = Just 1
    }

defenseMechanism :: CardDef
defenseMechanism = (identityUpgrade "27038" "Defense Mechanism" 1 [Superpower] [Mental])
    { cdDeckLimit = Just 1
    }

webShooterMilesMorales :: CardDef
webShooterMilesMorales = (identityUpgrade "27039" "Web-Shooter" 1 [Item, Tech] [Physical])
    { cdDeckLimit = Just 2
    }

symbioteSuit :: CardDef
symbioteSuit = (basicUpgrade "27191" "Symbiote Suit" 4 [Symbiote] [Wild])
    { cdDeckLimit = Just 1
    , cdHazards = 1
    }

-- Sinister Motives

smUpgradeDef :: CardCode -> Name -> SMSet.EncounterSet -> Natural -> CardDef
smUpgradeDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = UpgradeType
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

smCompactDartsA :: CardDef
smCompactDartsA = (smUpgradeDef "27182a" "Compact Darts" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Setup, SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smCompactDartsB :: CardDef
smCompactDartsB = (smUpgradeDef "27182b" "Compact Darts" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Enhanced, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smImpactDampeningSuitA :: CardDef
smImpactDampeningSuitA = (smUpgradeDef "27183a" "Impact-Dampening Suit" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Setup, SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smImpactDampeningSuitB :: CardDef
smImpactDampeningSuitB = (smUpgradeDef "27183b" "Impact-Dampening Suit" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Enhanced, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smLaserGogglesA :: CardDef
smLaserGogglesA = (smUpgradeDef "27184a" "Laser Goggles" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Setup, SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smLaserGogglesB :: CardDef
smLaserGogglesB = (smUpgradeDef "27184b" "Laser Goggles" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Enhanced, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smPropulsionGauntletA :: CardDef
smPropulsionGauntletA = (smUpgradeDef "27185a" "Propulsion Gauntlet" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Setup, SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smPropulsionGauntletB :: CardDef
smPropulsionGauntletB = (smUpgradeDef "27185b" "Propulsion Gauntlet" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Enhanced, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smRetinalDisplayA :: CardDef
smRetinalDisplayA = (smUpgradeDef "27186a" "Retinal Display" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Setup, SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smRetinalDisplayB :: CardDef
smRetinalDisplayB = (smUpgradeDef "27186b" "Retinal Display" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Enhanced, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smShockKnucklesA :: CardDef
smShockKnucklesA = (smUpgradeDef "27187a" "Shock Knuckles" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Setup, SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smShockKnucklesB :: CardDef
smShockKnucklesB = (smUpgradeDef "27187b" "Shock Knuckles" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Enhanced, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smWaveBracersA :: CardDef
smWaveBracersA = (smUpgradeDef "27188a" "Wave Bracers" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Setup, SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smWaveBracersB :: CardDef
smWaveBracersB = (smUpgradeDef "27188b" "Wave Bracers" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Enhanced, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smWristNavigatorA :: CardDef
smWristNavigatorA = (smUpgradeDef "27189a" "Wrist Navigator" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Setup, SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smWristNavigatorB :: CardDef
smWristNavigatorB = (smUpgradeDef "27189b" "Wrist Navigator" SMSet.ShieldTech 1)
  { cdTraits = fromList [SMTrait.Shield, SMTrait.Enhanced, SMTrait.Tech]
  , cdKeywords = fromList [SMKeyword.Permanent]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

-- Nova

supernovaHelmet :: CardDef
supernovaHelmet = (identityUpgrade "28009" "Supernova Helmet" 1 [Armor] [Mental])
  { cdDeckLimit = Just 1
  , cdUnique = True
  }

fluidMotion :: CardDef
fluidMotion = (upgrade "28016" "Fluid Motion" 1 [Skill] [Energy] Aggression)
  { cdDeckLimit = Just 3
  }

honedTechnique :: CardDef
honedTechnique = (upgrade "28017" "Honed Technique" 3 [Skill] [Mental] Aggression)
  { cdDeckLimit = Just 3
  , cdKeywords = fromList [Requirement [Mental, Mental]]
  , cdLimit = Just (MaxPerPlayer 1)
  }

heightAdvantage :: CardDef
heightAdvantage = (upgrade "28027" "Height Advantage" 1 [Aerial, Tactic] [Mental] Protection)
  { cdDeckLimit = Just 3
  }


-- Ironheart

photonBlasters :: CardDef
photonBlasters = (identityUpgrade "29012" "Photon Blasters" 2 [Tech, Weapon] [Mental])
  { cdDeckLimit = Just 1
  , cdUnique = False
  }

propulsionJets :: CardDef
propulsionJets = (identityUpgrade "29013" "Propulsion Jets" 2 [Tech] [Physical])
  { cdDeckLimit = Just 1
  , cdUnique = False
  }

ingenuity :: CardDef
ingenuity = (basicUpgrade "29027" "Ingenuity" 2 [Skill] [Mental])
  { cdDeckLimit = Just 3
  , cdUnique = False
  , cdCriteria = SelfMatches (IdentityWithTrait Genius)
  , cdLimit = Just (MaxPerPlayer 1)
  }


-- Spider-Ham

cartoonPhysics :: CardDef
cartoonPhysics = (identityUpgrade "30009" "Cartoon Physics" 1 [Cartoon, Superpower] [Physical])
  { cdDeckLimit = Just 2
  , cdUnique = False
  }

hugeWoodenHammer :: CardDef
hugeWoodenHammer = (identityUpgrade "30010" "Huge Wooden Hammer" 2 [Cartoon, Weapon] [Physical])
  { cdDeckLimit = Just 1
  , cdUnique = False
  }

organicWebbing :: CardDef
organicWebbing = (identityUpgrade "30011" "Organic Webbing" 2 [Cartoon, Superpower] [Mental])
  { cdDeckLimit = Just 1
  , cdUnique = False
  }

overwatch :: CardDef
overwatch = (upgrade "30019" "Overwatch" 0 [] [Physical] Justice)
  { cdDeckLimit = Just 3
  , cdUnique = False
  , cdLimit = Just (MaxPerScheme 1)
  }

warriorOfTheGreatWeb :: CardDef
warriorOfTheGreatWeb = (basicUpgrade "30029" "Warrior of the Great Web" 1 [Title] [Energy])
  { cdDeckLimit = Just 3
  , cdUnique = False
  , cdLimit = Just (MaxPerCharacter 1)
  }


-- SP//dr

spdrPilot :: CardDef
spdrPilot = (identityUpgrade "31002b" "SP//dr" 0 [Interface, Pilot] [Wild])
  { cdCost = Nothing
  , cdUnique = True
  , cdDeckLimit = Just 1
  , cdKeywords = fromList [SpdrKeyword.Permanent]
  }

hostSpider :: CardDef
hostSpider = (identityUpgrade "31010" "Host Spider" 3 [Interface, Spider] [Wild])
  { cdUnique = False
  , cdDeckLimit = Just 1
  }

psychicLink :: CardDef
psychicLink = (identityUpgrade "31011" "Psychic Link" 2 [Interface, Tech] [Mental])
  { cdUnique = False
  , cdDeckLimit = Just 1
  }

speedMetalAlloy :: CardDef
speedMetalAlloy = (identityUpgrade "31012" "Speed-Metal Alloy" 1 [Interface, Tech] [Physical])
  { cdUnique = False
  , cdDeckLimit = Just 1
  }

webFluidCompressor :: CardDef
webFluidCompressor = (identityUpgrade "31013" "Web-Fluid Compressor" 2 [Interface, Tech] [Energy])
  { cdUnique = False
  , cdDeckLimit = Just 1
  }

forcefieldGenerator :: CardDef
forcefieldGenerator = (upgrade "31019" "Forcefield Generator" 3 [Tech] [Energy] Protection)
  { cdUnique = False
  , cdDeckLimit = Just 3
  , cdLimit = Just (MaxPerPlayer 1)
  }

spiderTingle :: CardDef
spiderTingle = (upgrade "31020" "Spider-Tingle" 1 [Superpower] [Mental] Protection)
  { cdUnique = False
  , cdDeckLimit = Just 3
  }

unshakable :: CardDef
unshakable = (basicUpgrade "31024" "Unshakable" 1 [Condition, Superpower] [Physical])
  { cdUnique = False
  , cdCriteria = SelfMatches (IdentityWithPrintedHitPoints $ AtLeast $ SMValue.Static 14)
  , cdDeckLimit = Just 3
  }

clarityOfPurpose :: CardDef
clarityOfPurpose = (upgrade "31029" "Clarity of Purpose" 1 [Condition] [Mental] Leadership)
  { cdUnique = False
  , cdDeckLimit = Just 3
  , cdLimit = Just (MaxPerCharacter 1)
  }

-- Mutant Genesis

ironWill :: CardDef
ironWill = (identityUpgrade "32004" "Iron Will" 2 [Superpower] [Mental])
  { cdDeckLimit = Just 1
  }

-- Mutant Genesis

titaniumMuscles :: CardDef
titaniumMuscles = (identityUpgrade "32005" "Titanium Muscles" 2 [Superpower] [Physical])
  { cdDeckLimit = Just 1
  }

-- Mutant Genesis

organicSteel :: CardDef
organicSteel = (identityUpgrade "32006" "Organic Steel" 2 [Superpower] [Energy])
  { cdDeckLimit = Just 2
  }

-- Mutant Genesis

protectiveTraining :: CardDef
protectiveTraining = (upgrade "32013" "Protective Training" 1 [Training] [Mental] Protection)
  { cdDeckLimit = Just 3
  }

-- Mutant Genesis

solid :: CardDef
solid = (identityUpgrade "32031a" "Solid" 0 [] [Physical])
  { cdDeckLimit = Just 1
  , cdKeywords = fromList [Permanent, MassForm]
  , cdCost = Nothing
  }

-- Mutant Genesis

phased :: CardDef
phased = (identityUpgrade "32031b" "Phased" 0 [] [Mental])
  { cdDeckLimit = Just 1
  , cdKeywords = fromList [Permanent, MassForm]
  , cdCost = Nothing
  }

-- Mutant Genesis

acuteControl :: CardDef
acuteControl = (identityUpgrade "32034" "Acute Control" 1 [Superpower] [Mental])
  { cdDeckLimit = Just 1
  }

-- Mutant Genesis

intangibleInterference :: CardDef
intangibleInterference = (identityUpgrade "32035" "Intangible Interference" 1 [Superpower] [Mental])
  { cdDeckLimit = Just 1
  }

-- Mutant Genesis

phasedAndConfused :: CardDef
phasedAndConfused = (identityUpgrade "32036" "Phased and Confused" 3 [Condition] [Mental]) {cdCriteria = InHeroForm}
  { cdDeckLimit = Just 2
  }

-- Mutant Genesis

attackTraining :: CardDef
attackTraining = (upgrade "32043" "Attack Training" 1 [Training] [Physical] Aggression)
  { cdDeckLimit = Just 3
  }

-- Mutant Genesis

gatekeeper :: CardDef
gatekeeper = (upgrade "32044" "Gatekeeper" 0 [Condition] [Mental] Aggression)
  { cdDeckLimit = Just 3
  }

mgCoupDeGrace :: CardDef
mgCoupDeGrace = (basicUpgrade "32176" "Coup de Grâce" 0 [MGTrait.Skill] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgSwagger :: CardDef
mgSwagger = (basicUpgrade "32177" "Swagger" 0 [MGTrait.Skill] [MGResource.Wild]) {cdAbilitySubTypes = [Ability.Defense], cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgBrazenDefense :: CardDef
mgBrazenDefense = (basicUpgrade "32178" "Brazen Defense" 0 [MGTrait.Skill] [MGResource.Wild]) {cdAbilitySubTypes = [Ability.Attack, Ability.Defense], cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgFerociousAttack :: CardDef
mgFerociousAttack = (basicUpgrade "32179" "Ferocious Attack" 0 [MGTrait.Skill] [MGResource.Wild]) {cdAbilitySubTypes = [Ability.Attack], cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgWarCry :: CardDef
mgWarCry = (basicUpgrade "32180" "War Cry" 0 [MGTrait.Skill] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgCommanderCoupDeGrace :: CardDef
mgCommanderCoupDeGrace = (basicUpgrade "32181" "Coup de Grâce" 0 [MGTrait.Skill] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgCompassion :: CardDef
mgCompassion = (basicUpgrade "32182" "Compassion" 0 [MGTrait.Skill] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgGroupAssault :: CardDef
mgGroupAssault = (basicUpgrade "32183" "Group Assault" 0 [MGTrait.Skill, MGTrait.Tactic] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgShockAndAwe :: CardDef
mgShockAndAwe = (basicUpgrade "32184" "Shock and Awe" 0 [MGTrait.Skill, MGTrait.Tactic] [MGResource.Wild]) {cdAbilitySubTypes = [Ability.Attack], cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgImprovisation :: CardDef
mgImprovisation = (basicUpgrade "32185" "Improvisation" 0 [MGTrait.Skill] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgDefenderSwagger :: CardDef
mgDefenderSwagger = (basicUpgrade "32186" "Swagger" 0 [MGTrait.Skill] [MGResource.Wild]) {cdAbilitySubTypes = [Ability.Defense], cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgSurprise :: CardDef
mgSurprise = (basicUpgrade "32187" "Surprise!" 0 [MGTrait.Skill] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgHeroicIntervention :: CardDef
mgHeroicIntervention = (basicUpgrade "32188" "Heroic Intervention" 0 [MGTrait.Skill] [MGResource.Wild]) {cdAbilitySubTypes = [Ability.Thwart], cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgDeterminedDefense :: CardDef
mgDeterminedDefense = (basicUpgrade "32189" "Determined Defense" 0 [MGTrait.Skill] [MGResource.Wild]) {cdAbilitySubTypes = [Ability.Defense, Ability.Thwart], cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgBodyguard :: CardDef
mgBodyguard = (basicUpgrade "32190" "Bodyguard" 0 [MGTrait.Skill] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgPeacekeeperSurprise :: CardDef
mgPeacekeeperSurprise = (basicUpgrade "32191" "Surprise!" 0 [MGTrait.Skill] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgPeacekeeperCompassion :: CardDef
mgPeacekeeperCompassion = (basicUpgrade "32192" "Compassion" 0 [MGTrait.Skill] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgRescueOperation :: CardDef
mgRescueOperation = (basicUpgrade "32193" "Rescue Operation" 0 [MGTrait.Skill, MGTrait.Tactic] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgMentorship :: CardDef
mgMentorship = (basicUpgrade "32194" "Mentorship" 0 [MGTrait.Skill, MGTrait.Tactic] [MGResource.Wild]) {cdAbilitySubTypes = [Ability.Thwart], cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}

mgFortitude :: CardDef
mgFortitude = (basicUpgrade "32195" "Fortitude" 0 [MGTrait.Skill] [MGResource.Wild]) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1}


-- Phoenix

phoenixForceRestrained :: CardDef
phoenixForceRestrained = (identityUpgrade "34002a" "Phoenix Force" 0 [Restrained] [Mental]) {cdDeckLimit = Just 1, cdKeywords = fromList [Permanent], cdCost = Nothing}

phoenixForceUnleashed :: CardDef
phoenixForceUnleashed = (identityUpgrade "34002b" "Phoenix Force" 0 [Unleashed] [Physical]) {cdDeckLimit = Just 1, cdKeywords = fromList [Permanent], cdCost = Nothing}

phoenixSuit :: CardDef
phoenixSuit = (identityUpgrade "34005" "Phoenix Suit" 2 [Item] [Physical]) {cdDeckLimit = Just 1, cdUnique = True}

riseFromTheAshes :: CardDef
riseFromTheAshes = (identityUpgrade "34006" "Rise from the Ashes" 3 [Phoenix] [Energy]) {cdDeckLimit = Just 1}

telekineticShield :: CardDef
telekineticShield = (identityUpgrade "34007" "Telekinetic Shield" 2 [Psionic] [Physical]) {cdDeckLimit = Just 1}

mentalParalysis :: CardDef
mentalParalysis = (identityUpgrade "34008" "Mental Paralysis" 2 [Psionic] [Mental]) {cdDeckLimit = Just 1, cdCriteria = InHeroForm}

mindControl :: CardDef
mindControl = (identityUpgrade "34009" "Mind Control" 4 [Condition, Psionic] [Mental]) {cdDeckLimit = Just 1}

missionTraining :: CardDef
missionTraining = (upgrade "34016" "Mission Training" 1 [Training] [Mental] Justice) {cdDeckLimit = Just 3}


-- Cyclops

rubyQuartzVisor :: CardDef
rubyQuartzVisor = (identityUpgrade "33003" "Ruby Quartz Visor" 2 [Item] [Physical]) { cdDeckLimit = Just 1, cdUnique = True }

fieldCommander :: CardDef
fieldCommander = (identityUpgrade "33004" "Field Commander" 1 [Title] [Physical]) { cdDeckLimit = Just 1, cdUnique = True }

exploitWeakness :: CardDef
exploitWeakness = (identityUpgrade "33005" "Exploit Weakness" 1 [Tactic] [Physical]) { cdDeckLimit = Just 2, cdKeywords = fromList [Temporary], cdLimit = Just (MaxPerEnemy 1) }

practicedDefense :: CardDef
practicedDefense = (identityUpgrade "33006" "Practiced Defense" 0 [Tactic] [Energy]) { cdDeckLimit = Just 2, cdKeywords = fromList [Temporary], cdLimit = Just (MaxPerEnemy 1) }

priorityTarget :: CardDef
priorityTarget = (identityUpgrade "33007" "Priority Target" 0 [Tactic] [Mental]) { cdDeckLimit = Just 2, cdKeywords = fromList [Temporary], cdLimit = Just (MaxPerEnemy 1) }

dangerRoomTraining :: CardDef
dangerRoomTraining = (upgrade "33015" "Danger Room Training" 1 [Training] [Mental] Leadership) { cdDeckLimit = Just 3 }

coordinatedAttack :: CardDef
coordinatedAttack = (upgrade "33016" "Coordinated Attack" 0 [Tactic] [Physical] Leadership) { cdDeckLimit = Just 3, cdLimit = Just (MaxPerEnemy 1) }

marked :: CardDef
marked = (upgrade "33032" "Marked" 0 [Tactic] [Physical] Aggression) { cdDeckLimit = Just 3, cdLimit = Just (MaxPerEnemy 1) }

befuddle :: CardDef
befuddle = (upgrade "33033" "Befuddle" 0 [Tactic] [Physical] Justice) { cdDeckLimit = Just 3, cdLimit = Just (MaxPerEnemy 1) }

pinnedDown :: CardDef
pinnedDown = (upgrade "33034" "Pinned Down" 0 [Tactic] [Physical] Protection) { cdDeckLimit = Just 3, cdLimit = Just (MaxPerEnemy 1) }

honoraryXMen :: CardDef
honoraryXMen = (basicUpgrade "33035" "Honorary X-Men" 0 [Title] [Physical]) { cdDeckLimit = Just 3, cdLimit = Just (MaxPerCharacter 1), cdCriteria = SelfMatches (IdentityWithTrait XMen) }


-- Storm

stormsCrown :: CardDef
stormsCrown = (identityUpgrade "36006" "Storm's Crown" 2 [Item] [Mental]) { cdUnique = True, cdDeckLimit = Just 1 }

stormsCape :: CardDef
stormsCape = (identityUpgrade "36007" "Storm's Cape" 3 [Item] [Energy]) { cdUnique = True, cdDeckLimit = Just 1 }

leadershipSkill :: CardDef
leadershipSkill = (upgrade "36019" "Leadership Skill" 1 [Skill] [Physical] Leadership) { cdDeckLimit = Just 3, cdLimit = Just (MaxPerPlayer 1) }

-- Wolverine

wolverinesClaws :: CardDef
wolverinesClaws = (identityUpgrade "35002" "Wolverine's Claws" 0 [Weapon] [Physical]) {cdUnique = True, cdDeckLimit = Just 1, cdCost = Nothing, cdKeywords = fromList [Permanent]}

adamantiumSkeleton :: CardDef
adamantiumSkeleton = (identityUpgrade "35004" "Adamantium Skeleton" 2 [] [Physical]) {cdDeckLimit = Just 1}

berserkerFrenzy :: CardDef
berserkerFrenzy = (identityUpgrade "35005" "Berserker Frenzy" 2 [Condition] [Mental]) {cdDeckLimit = Just 1}

iGotBetter :: CardDef
iGotBetter = (identityUpgrade "35006" "\"I Got Better\"" 4 [Superpower] [Energy]) {cdDeckLimit = Just 1}

warriorSkill :: CardDef
warriorSkill = (upgrade "35016" "Warrior Skill" 2 [Skill] [Mental] Aggression) {cdDeckLimit = Just 3, cdLimit = Just (MaxPerPlayer 1)}
