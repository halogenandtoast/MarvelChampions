module Marvel.SideScheme.Cards where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Name

allSideSchemes :: HashMap CardCode CardDef
allSideSchemes =
  fromList $
    map
      (toFst toCardCode)
      [ breakinAndTakin
      , crowdControl
      , bombScare
      , defenseNetwork
      , illegalArmsFactory
      , theImmortalKlaw
      , theMastersOfEvil
      , usurpTheThrone
      , personalChallenge
      , highwayRobbery
      , imminentOverload
      ]

sideScheme ::
  CardCode -> Name -> [BoostIcon] -> EncounterSet -> Natural -> CardDef
sideScheme code name boostIcons encounterSet quantity =
  CardDef
    { cdCardCode = code
    , cdName = name
    , cdCost = Nothing
    , cdTraits = mempty
    , cdKeywords = mempty
    , cdCardType = SideSchemeType
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
    }

breakinAndTakin :: CardDef
breakinAndTakin =
  (sideScheme "01107" "Breakin' & Takin'" [Boost, Boost] Rhino 1)
    { cdHazards = 1
    }

crowdControl :: CardDef
crowdControl = sideScheme "01108" "Crowd Control" [Boost, Boost] Rhino 1

bombScare :: CardDef
bombScare =
  (sideScheme "01109" "Bomb Scare" [Boost, Boost] BombScare 1)
    { cdAcceleration = 1
    }

defenseNetwork :: CardDef
defenseNetwork = sideScheme "01125" "Defense Network" [Boost, Boost] Klaw 1

illegalArmsFactory :: CardDef
illegalArmsFactory =
  (sideScheme "01126" "Illegal Arms Factory" [Boost, Boost] Klaw 1)
    { cdHazards = 1
    }

theImmortalKlaw :: CardDef
theImmortalKlaw =
  (sideScheme "01127" "The \"Immortal\" Klaw" [] Klaw 1)
    { cdAcceleration = 1
    }

theMastersOfEvil :: CardDef
theMastersOfEvil =
  (sideScheme "01128" "The Masters of Evil" [Boost, Boost] MastersOfEvil 1)
    { cdAcceleration = 1
    }

usurpTheThrone :: CardDef
usurpTheThrone =
  ( sideScheme
      "01156"
      "Usurp the Throne"
      [Boost, Boost, Boost]
      BlackPantherNemesis
      1
  )
    { cdHazards = 1
    }

personalChallenge :: CardDef
personalChallenge =
  sideScheme "01161" "Personal Challenge" [Boost, Boost, Boost] SheHulkNemesis 1

highwayRobbery :: CardDef
highwayRobbery =
  ( sideScheme "01166" "Highway Robbery" [Boost, Boost, Boost] SpiderManNemesis 1
  )
    { cdAcceleration = 1
    }

imminentOverload :: CardDef
imminentOverload =
  ( sideScheme "01171" "Imminent Overload" [Boost, Boost, Boost] IronManNemesis 1
  )
    { cdAcceleration = 1
    }
