module Marvel.Upgrade.Cards where

import Marvel.Prelude

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Name
import Marvel.Resource
import Marvel.Trait

allUpgrades :: HashMap CardCode CardDef
allUpgrades =
  fromList $
    map
      (toCardCode &&& id)
      [ spiderTracer
      , webShooter
      , webbedUp
      , captainMarvelsHelmet
      , cosmicFlight
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
      ]

upgrade ::
  CardCode -> Name -> Int -> [Trait] -> [Resource] -> Aspect -> CardDef
upgrade code name cost traits resources aspect =
  baseUpgrade code name cost traits resources (Just aspect)

identityUpgrade :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
identityUpgrade code name cost traits resources =
  baseUpgrade code name cost traits resources Nothing

basicUpgrade :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
basicUpgrade code name cost traits resources =
  baseUpgrade code name cost traits resources Nothing

baseUpgrade ::
  CardCode -> Name -> Int -> [Trait] -> [Resource] -> Maybe Aspect -> CardDef
baseUpgrade code name cost traits resources mAspect =
  CardDef
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
    , cdResources = map (PrintedResource,) resources
    , cdResponseWindow = Nothing
    , cdBoostIcons = []
    , cdHazards = 0
    , cdAcceleration = 0
    }

unique :: CardDef -> CardDef
unique def = def {cdUnique = True}

spiderTracer :: CardDef
spiderTracer =
  ( identityUpgrade "01007" "Spider-Tracer" 1 [Item, Tech] [Energy]
  )
    { cdCriteria = MinionExists AnyMinion
    }

webShooter :: CardDef
webShooter = identityUpgrade "01008" "Web-Shooter" 1 [Item, Tech] [Physical]

webbedUp :: CardDef
webbedUp =
  (identityUpgrade "01009" "Webbed Up" 4 [Condition] [Physical])
    { cdCriteria = InHeroForm
    }

captainMarvelsHelmet :: CardDef
captainMarvelsHelmet =
  unique $ identityUpgrade "01016" "Captain Marvel's Helmet" 2 [Armor, Tech] [Physical]

cosmicFlight :: CardDef
cosmicFlight =
  identityUpgrade "01017" "Cosmic Flight" 2 [Superpower] [Energy]

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
  identityUpgrade "01038" "Power Gauntlets" 2 [Armor, Tech] [Energy]

rocketBoots :: CardDef
rocketBoots =
  identityUpgrade "01039" "Rocket Boots" 1 [Armor, Tech] [Mental]

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
combatTraining = upgrade "01057" "Combat Training" 2 [Skill] [Physical] Aggression

heroicIntuition :: CardDef
heroicIntuition = upgrade "01065" "Heroic Intuition" 2 [Skill] [Energy] Justice

inspired :: CardDef
inspired =
  (upgrade "01074" "Inspired" 1 [Condition] [Physical] Leadership)
    { cdCriteria = AllyExists AnyAlly
    }

armoredVest :: CardDef
armoredVest = upgrade "01081" "Armored Vest" 1 [Armor] [Mental] Protection

indomitable :: CardDef
indomitable = upgrade "01082" "Indomitable" 1 [Condition] [Energy] Protection

tenacity :: CardDef
tenacity = basicUpgrade "01093" "Tenacity" 2 [Condition] [Energy]
