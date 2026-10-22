module Marvel.SideScheme.Cards where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Name

allSideSchemes :: HashMap CardCode CardDef
allSideSchemes = fromList $ map
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
  , thePsycheMagnitron
  , droneFactory
  , invasiveAI
  , ultronsImperative
  , underAttack
  , legionsOfHydra
  , theDoomsdayChair
  , hitSquad
  , generationWhy
  , killerForHire
  , familyFeud
  , openTheDarkDimension
  , totalDestruction
  ]

sideScheme
  :: CardCode -> Name -> [BoostIcon] -> EncounterSet -> Natural -> CardDef
sideScheme code name boostIcons encounterSet quantity = CardDef
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
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  }

breakinAndTakin :: CardDef
breakinAndTakin =
  (sideScheme "01107" "Breakin' & Takin'" [Boost, Boost] Rhino 1)
    { cdHazards = 1
    }

crowdControl :: CardDef
crowdControl = sideScheme "01108" "Crowd Control" [Boost, Boost] Rhino 1

bombScare :: CardDef
bombScare = (sideScheme "01109" "Bomb Scare" [Boost, Boost] BombScare 1)
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
  (sideScheme "01127" "The \"Immortal\" Klaw" [] Klaw 1) { cdAcceleration = 1 }

theMastersOfEvil :: CardDef
theMastersOfEvil =
  (sideScheme "01128" "The Masters of Evil" [Boost, Boost] MastersOfEvil 1)
    { cdAcceleration = 1
    }

usurpTheThrone :: CardDef
usurpTheThrone = (sideScheme
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
  (sideScheme "01166" "Highway Robbery" [Boost, Boost, Boost] SpiderManNemesis 1
    )
    { cdAcceleration = 1
    }

imminentOverload :: CardDef
imminentOverload =
  (sideScheme "01171" "Imminent Overload" [Boost, Boost, Boost] IronManNemesis 1
    )
    { cdAcceleration = 1
    }

thePsycheMagnitron :: CardDef
thePsycheMagnitron = (sideScheme
                       "01176"
                       "The Psyche-Magnitron"
                       [Boost, Boost, Boost]
                       CaptainMarvelNemesis
                       1
                     )
  { cdHazards = 1
  }

droneFactory :: CardDef
droneFactory =
  (sideScheme "01148" "Drone Factory" [Boost, Boost] Ultron 1) {cdAcceleration = 1}

invasiveAI :: CardDef
invasiveAI =
  (sideScheme "01149" "Invasive AI" [Boost, Boost, Boost] Ultron 1) {cdHazards = 1}

ultronsImperative :: CardDef
ultronsImperative =
  (sideScheme "01150" "Ultron's Imperative" [Boost, Boost, Boost] Ultron 1) {cdHazards = 1}

underAttack :: CardDef
underAttack = sideScheme "01151" "Under Attack" [Boost, Boost, Boost] UnderAttack 1

legionsOfHydra :: CardDef
legionsOfHydra =
  (sideScheme "01180" "Legions of Hydra" [Boost, Boost, Boost] LegionsOfHydra 2) {cdHazards = 1}

theDoomsdayChair :: CardDef
theDoomsdayChair =
  (sideScheme "01183" "The Doomsday Chair" [Boost, Boost, Boost] TheDoomsdayChair 2)
    { cdAcceleration = 1
    }

hitSquad :: CardDef
hitSquad =
  (sideScheme "03027" "Hit Squad" [Boost, Boost, Boost] CaptainAmericaNemesis 1)
    { cdAcceleration = 1
    }

generationWhy :: CardDef
generationWhy =
  (sideScheme "05026" "Generation Why?" [Boost, Boost, Boost] MsMarvelNemesis 1)
totalDestruction :: CardDef
totalDestruction =
  (sideScheme "10027" "Total Destruction" [Boost, Boost, Boost] HulkNemesis 1)
    { cdHazards = 1
    }

killerForHire :: CardDef
killerForHire =
  (sideScheme "08027" "Killer for Hire" [Boost, Boost, Boost] BlackWidowNemesis 1)
    { cdAcceleration = 1
    }

familyFeud :: CardDef
familyFeud = (sideScheme "06027" "Family Feud" [Boost, Boost, Boost] ThorNemesis 1)
  { cdAcceleration = 1
  }

openTheDarkDimension :: CardDef
openTheDarkDimension =
  (sideScheme "09029" "Open the Dark Dimension" [Boost, Boost, Boost] DoctorStrangeNemesis 1)
    { cdAcceleration = 1
    }
