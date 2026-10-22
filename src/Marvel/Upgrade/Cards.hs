module Marvel.Upgrade.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Data.HashSet (singleton)
import Marvel.Criteria
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Name
import Marvel.Resource
import Marvel.Trait

allUpgrades :: HashMap CardCode CardDef
allUpgrades = fromList $ concatMap
  toCardCodePairs
  [ spiderTracer
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

  -- The Once and Future Kang

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
  , cdAbilitySubType = Nothing
  , cdUnique = False
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = map (PrintedResource, ) resources
  , cdResponseWindow = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdLimit = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
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
combatTraining = reprintedAs ["04041"] $ limited (MaxPerPlayerUnderAnyControl 1)
  $ upgrade "01057" "Combat Training" 2 [Skill] [Physical] Aggression

heroicIntuition :: CardDef
heroicIntuition = reprintedAs ["04046"] $ limited (MaxPerPlayerUnderAnyControl 1)
  $ upgrade "01065" "Heroic Intuition" 2 [Skill] [Energy] Justice

inspired :: CardDef
inspired = (upgrade "01074" "Inspired" 1 [Condition] [Physical] Leadership)
  { cdCriteria = AllyExists AnyAlly
  , cdLimit = Just (MaxPerAlly 1)
  }

armoredVest :: CardDef
armoredVest = limited (MaxPerPlayerUnderAnyControl 1)
  $ upgrade "01081" "Armored Vest" 1 [Armor] [Mental] Protection

indomitable :: CardDef
indomitable = upgrade "01082" "Indomitable" 1 [Condition] [Energy] Protection

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
followed = limited (MaxPerSideScheme 1) $
  upgrade "03032" "Followed" 1 [] [Energy] Justice

enhancedAwareness :: CardDef
enhancedAwareness =
  basicUpgrade "03034" "Enhanced Awareness" 2 [Condition] [Mental]

biokineticPolymerSuit :: CardDef
biokineticPolymerSuit =
  unique $ identityUpgrade "05009" "Biokinetic Polymer Suit" 1 [Armor] [Mental]

embiggen :: CardDef
embiggen = identityUpgrade "05010" "Embiggen!" 2 [Condition, Superpower] [Energy]

shrink :: CardDef
shrink = identityUpgrade "05011" "Shrink" 2 [Condition, Superpower] [Mental]

energyBarrier :: CardDef
energyBarrier = upgrade "05017" "Energy Barrier" 2 [Item, Tech] [Energy] Protection

endurance :: CardDef
endurance = limited (MaxPerPlayerUnderAnyControl 1)
  $ basicUpgrade "05023" "Endurance" 1 [Condition] [Energy]

enhancedReflexes :: CardDef
enhancedReflexes = basicUpgrade "05024" "Enhanced Reflexes" 2 [Condition] [Energy]

downTime :: CardDef
downTime = limited (MaxPerPlayerUnderAnyControl 1)
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
resourceful = basicUpgrade "10032" "Resourceful" 1 [Skill] [Wild]

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
battleFury = limited (MaxPerPlayerUnderAnyControl 1)
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
enhancedPhysique = basicUpgrade "06034" "Enhanced Physique" 2 [Condition] [Physical]

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
