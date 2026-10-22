module Marvel.Upgrade.Cards where

import Marvel.Prelude

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
  , boundlessRage
  , immovableObject
  , martialProwess
  , electrostaticArmor
  , resourceful
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
  baseUpgrade code name cost traits resources Nothing

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
combatTraining = limited (MaxPerPlayerUnderAnyControl 1)
  $ upgrade "01057" "Combat Training" 2 [Skill] [Physical] Aggression

heroicIntuition :: CardDef
heroicIntuition = limited (MaxPerPlayerUnderAnyControl 1)
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
