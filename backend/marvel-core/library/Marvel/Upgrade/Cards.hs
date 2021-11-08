module Marvel.Upgrade.Cards where

import Marvel.Prelude

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Name
import Marvel.Resource
import Marvel.Trait

allUpgrades :: HashMap CardCode CardDef
allUpgrades = fromList
  $ map (toCardCode &&& id) [spiderTracer, webShooter, webbedUp, inspired]

upgrade
  :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> Aspect -> CardDef
upgrade code name cost traits resources aspect =
  baseUpgrade code name cost traits resources (Just aspect)

identityUpgrade :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> CardDef
identityUpgrade code name cost traits resources =
  baseUpgrade code name cost traits resources Nothing

baseUpgrade
  :: CardCode -> Name -> Int -> [Trait] -> [Resource] -> Maybe Aspect -> CardDef
baseUpgrade code name cost traits resources mAspect = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Just cost
  , cdTraits = fromList traits
  , cdCardType = SupportType
  , cdUnique = False
  , cdAspect = mAspect
  , cdResources = map (PrintedResource, ) resources
  }

spiderTracer :: CardDef
spiderTracer = identityUpgrade "01007" "Spider-Tracer" 1 [Item, Tech] [Energy]

webShooter :: CardDef
webShooter = identityUpgrade "01008" "Web-Shooter" 1 [Item, Tech] [Physical]

webbedUp :: CardDef
webbedUp = identityUpgrade "01009" "Webbed Up" 4 [Condition] [Physical]

inspired :: CardDef
inspired = upgrade "01074" "Inspired" 1 [Condition] [Physical] Leadership
