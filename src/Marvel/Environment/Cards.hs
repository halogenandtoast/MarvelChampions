module Marvel.Environment.Cards where

import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.Trait qualified as MGTrait
import Marvel.Resource qualified as MGResource
import Marvel.Boost qualified as MGBoost
import Marvel.Keyword qualified as SpdrKeyword
import Marvel.EncounterSet qualified as SpdrSet
import Marvel.EncounterSet qualified as SMSet
import Marvel.Trait qualified as SMTrait
import Marvel.Keyword qualified as SMKeyword
import Marvel.GameValue.Types qualified as SMValue
import Marvel.Boost qualified as SMBoost

import Marvel.Trait qualified as MGTrait
import Marvel.EncounterSet qualified as MGSet
import Marvel.Keyword qualified as MGKeyword
import Marvel.GameValue.Types qualified as MGValue
import Marvel.Boost qualified as MGBoost
import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Data.HashSet (singleton)
import Marvel.Boost
import Marvel.Keyword
import Marvel.Name
import Marvel.Trait

allEnvironments :: HashMap CardCode CardDef
allEnvironments = fromList $ map (toFst toCardCode)
  [ mgProtectTheSenator, mgSaveTheSchool, mgRescueCaptives
  , smCityStreets, smBellTowerA, smBellTowerB, smSkiesOverNewYorkB, smLowerManhattanB, smMidtownManhattanB, smUpperManhattanB, smPublicOutcryA, smPublicOutcryB
  , ultronDrones
  -- The Green Goblin
  , criminalEnterprise
  , stateOfMadness

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , denseForest
  , snowyHillside
  , rockyOutcrop
  , abandonedFacility

  -- The Once and Future Kang

  -- The Galaxy's Most Wanted: Ship Command
  , rogueVessel
  -- Brotherhood of Badoon
  , badoonShip

  -- The Galaxy's Most Wanted: Escape the Museum
  , libraryLabyrinth
  , museumShip
  -- Ronan the Accuser
  , kreeCommandShip

  -- The Galaxy's Most Wanted: Nebula
  , nebulasShip

  -- The Mad Titan's Shadow: Ebony Maw
  , fireball
  , manipulation
  , pacification
  , rubblestorm

  -- The Mad Titan's Shadow: Tower Defense
  , avengersTowerStronghold
  , avengersTowerDamaged

  -- The Mad Titan's Shadow: Infinity Gauntlet
  , mindStone
  , infinityPowerStone
  , realityStone
  , soulStone
  , spaceStone
  , timeStone
  , ultronDronesVision
  , airStatic
  , surgeInCrime
  ]

-- | Ultron Drones is put into play by The Crimson Cowl's setup, so none are shuffled
-- into the encounter deck.
ultronDrones :: CardDef
ultronDrones =
  CardDef
    { cdCardCode = "01140"
    , cdName = "Ultron Drones"
    , cdCost = Nothing
    , cdTraits = mempty
    , cdKeywords = mempty
    , cdCardType = EnvironmentType
    , cdAbilityType = Nothing
    , cdAbilitySubTypes = []
    , cdUnique = False
    , cdAspect = Nothing
    , cdEncounterSet = Just Ultron
    , cdEncounterSetQuantity = Just 0
    , cdCriteria = NoCriteria
    , cdResources = []
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

-- The Green Goblin

-- | Put into play by Hostile Takeover's setup, so none is shuffled into the encounter deck.
criminalEnterprise :: CardDef
criminalEnterprise = ultronDrones {cdCardCode = "02006a", cdName = "Criminal Enterprise", cdEncounterSet = Just RiskyBusiness}

stateOfMadness :: CardDef
stateOfMadness = criminalEnterprise {cdCardCode = "02006b", cdName = "State of Madness"}
-- | Absorbing Man's environments, each with surge and a boost to put it into play.
absorbingEnvironment :: CardCode -> Name -> Trait -> CardDef
absorbingEnvironment code name trait =
  ultronDrones
    { cdCardCode = code
    , cdName = name
    , cdTraits = singleton trait
    , cdKeywords = singleton Surge
    , cdEncounterSet = Just AbsorbingMan
    , cdEncounterSetQuantity = Just 1
    , cdBoostIcons = [Star]
    }

denseForest :: CardDef
denseForest = absorbingEnvironment "04080" "Dense Forest" Wood

snowyHillside :: CardDef
snowyHillside = absorbingEnvironment "04081" "Snowy Hillside" Ice

rockyOutcrop :: CardDef
rockyOutcrop = absorbingEnvironment "04082" "Rocky Outcrop" Stone

abandonedFacility :: CardDef
abandonedFacility = absorbingEnvironment "04083" "Abandoned Facility" Metal

-- The Galaxy's Most Wanted: Ship Command

rogueVessel :: CardDef
rogueVessel =
  ultronDrones
    { cdCardCode = "16143"
    , cdName = "Rogue Vessel"
    , cdTraits = fromList [Aerial, Vehicle]
    , cdKeywords = singleton Surge
    , cdEncounterSet = Just ShipCommand
    , cdEncounterSetQuantity = Just 1
    , cdBoostIcons = [Boost, Boost]
    }

-- The Galaxy's Most Wanted: Brotherhood of Badoon

-- | Put into play by Terrestrial Invasion's setup, so none is shuffled into the encounter
-- deck.
badoonShip :: CardDef
badoonShip =
  ultronDrones
    { cdCardCode = "16063"
    , cdName = "Badoon Ship"
    , cdTraits = fromList [Aerial, Vehicle]
    , cdUnique = True
    , cdEncounterSet = Just BrotherhoodOfBadoon
    }

-- The Galaxy's Most Wanted: Escape the Museum

-- | Put into play by The Missing Milano's setup, so none is shuffled into the encounter
-- deck; The Great Escape flips it to Museum Ship.
libraryLabyrinth :: CardDef
libraryLabyrinth =
  ultronDrones
    { cdCardCode = "16085a"
    , cdName = "Library Labyrinth"
    , cdTraits = singleton Location
    , cdUnique = True
    , cdEncounterSet = Just EscapeTheMuseum
    }

museumShip :: CardDef
museumShip = libraryLabyrinth {cdCardCode = "16085b", cdName = "Museum Ship", cdTraits = fromList [Aerial, Vehicle]}

-- Ronan the Accuser

kreeCommandShip :: CardDef
kreeCommandShip =
  ultronDrones
    { cdCardCode = "16108"
    , cdName = "Kree Command Ship"
    , cdTraits = fromList [Aerial, Vehicle]
    , cdUnique = True
    , cdEncounterSet = Just Ronan
    , cdEncounterSetQuantity = Just 1
    }

-- The Galaxy's Most Wanted: Nebula

-- | Put into play by The Art of Evasion's setup, so none is shuffled into the encounter
-- deck.
nebulasShip :: CardDef
nebulasShip =
  ultronDrones
    { cdCardCode = "16093"
    , cdName = "Nebula's Ship"
    , cdTraits = fromList [Aerial, Vehicle]
    , cdUnique = True
    , cdEncounterSet = Just Nebula
    , cdEncounterSetQuantity = Just 0
    }

-- The Mad Titan's Shadow: Tower Defense

-- | Put into play by The Armies of Thanos (2A), so none is shuffled into the encounter
-- deck. "A unique, double-sided environment card" (MTS rulebook p.11).
avengersTowerStronghold :: CardDef
avengersTowerStronghold =
  ultronDrones
    { cdCardCode = "21100a"
    , cdName = "Avengers Tower"
    , cdTraits = singleton Stronghold
    , cdUnique = True
    , cdEncounterSet = Just TowerDefense
    , cdEncounterSetQuantity = Just 0
    }

avengersTowerDamaged :: CardDef
avengersTowerDamaged = avengersTowerStronghold {cdCardCode = "21100b", cdTraits = singleton Damaged}

-- The Mad Titan's Shadow: Ebony Maw

-- | Ebony Maw's Spells: surge, boost 2, two of each.
spell :: CardCode -> Name -> CardDef
spell code name =
  ultronDrones
    { cdCardCode = code
    , cdName = name
    , cdTraits = singleton Spell
    , cdKeywords = singleton Surge
    , cdEncounterSet = Just EbonyMaw
    , cdEncounterSetQuantity = Just 2
    , cdBoostIcons = [Boost, Boost]
    }

fireball :: CardDef
fireball = spell "21076" "Fireball"

manipulation :: CardDef
manipulation = spell "21077" "Manipulation"

pacification :: CardDef
pacification = spell "21078" "Pacification"

rubblestorm :: CardDef
rubblestorm = spell "21079" "Rubblestorm"

-- The Mad Titan's Shadow: Infinity Gauntlet

-- | An Infinity Stone: set aside as the Infinity Stone deck by the Infinity Gauntlet, so
-- none is in the encounter deck once it is attached.
infinityStone :: CardCode -> Name -> Natural -> CardDef
infinityStone code name boosts =
  ultronDrones
    { cdCardCode = code
    , cdName = name
    , cdTraits = singleton InfinityStone
    , cdEncounterSet = Just InfinityGauntlet
    , cdEncounterSetQuantity = Just 1
    , cdBoostIcons = replicate (fromIntegral boosts) Boost
    }

mindStone :: CardDef
mindStone = infinityStone "21130" "Mind Stone" 2

-- | The other Power Stone (16149) is The Galaxy's Most Wanted's attachment.
infinityPowerStone :: CardDef
infinityPowerStone = infinityStone "21131" "Power Stone" 3

realityStone :: CardDef
realityStone = infinityStone "21132" "Reality Stone" 3

soulStone :: CardDef
soulStone = infinityStone "21133" "Soul Stone" 1

spaceStone :: CardDef
spaceStone = infinityStone "21134" "Space Stone" 2

timeStone :: CardDef
timeStone = infinityStone "21135" "Time Stone" 4

-- Vision

ultronDronesVision :: CardDef
ultronDronesVision = (ultronDrones)
    { cdCardCode = "26031"
    , cdName = "Ultron Drones"
    , cdEncounterSet = Just VisionNemesis
    , cdEncounterSetQuantity = Just 1
    , cdBoostIcons = []
    }

-- Sinister Motives

smEnvironmentDef :: CardCode -> Name -> SMSet.EncounterSet -> Natural -> CardDef
smEnvironmentDef code name set quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = EnvironmentType
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

smCityStreets :: CardDef
smCityStreets = (smEnvironmentDef "27065" "City Streets" SMSet.Sandman 1)
  { cdTraits = fromList [SMTrait.Location]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smBellTowerA :: CardDef
smBellTowerA = (smEnvironmentDef "27077a" "Bell Tower" SMSet.Venom 0)
  { cdTraits = fromList [SMTrait.Location, SMTrait.Quiet]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smBellTowerB :: CardDef
smBellTowerB = (smEnvironmentDef "27077b" "Bell Tower" SMSet.Venom 0)
  { cdTraits = fromList [SMTrait.Location, SMTrait.Ringing]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smSkiesOverNewYorkB :: CardDef
smSkiesOverNewYorkB = (smEnvironmentDef "27116b" "Skies Over New York" SMSet.VenomGoblin 0)
  { cdTraits = fromList []
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smLowerManhattanB :: CardDef
smLowerManhattanB = (smEnvironmentDef "27117b" "Lower Manhattan" SMSet.VenomGoblin 0)
  { cdTraits = fromList [SMTrait.Location, SMTrait.Symbiote]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smMidtownManhattanB :: CardDef
smMidtownManhattanB = (smEnvironmentDef "27118b" "Midtown Manhattan" SMSet.VenomGoblin 0)
  { cdTraits = fromList [SMTrait.Location, SMTrait.Symbiote]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smUpperManhattanB :: CardDef
smUpperManhattanB = (smEnvironmentDef "27119b" "Upper Manhattan" SMSet.VenomGoblin 0)
  { cdTraits = fromList [SMTrait.Location, SMTrait.Symbiote]
  , cdKeywords = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  }

smPublicOutcryA :: CardDef
smPublicOutcryA = (smEnvironmentDef "27174a" "Public Outcry" SMSet.BadPublicity 0)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Victory 1]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 1
  , cdAmplify = 0
  }

smPublicOutcryB :: CardDef
smPublicOutcryB = (smEnvironmentDef "27174b" "Public Outcry" SMSet.BadPublicity 0)
  { cdTraits = fromList []
  , cdKeywords = fromList [SMKeyword.Victory 1]
  , cdUnique = False
  , cdBoostIcons = []
  , cdHazards = 1
  , cdAcceleration = 0
  , cdAmplify = 0
  }


-- Ironheart

airStatic :: CardDef
airStatic = (smEnvironmentDef "29039" "Air Static" SMSet.ZzzaxSet 1)
  { cdTraits = fromList [Condition]
  , cdBoostIcons = [Boost, Boost]
  , cdUnique = False
  }


-- SP//dr

surgeInCrime :: CardDef
surgeInCrime = (smEnvironmentDef "31037" "Surge in Crime" SpdrSet.IronSpiderSinisterSix 1)
  { cdBoostIcons = [Boost, Boost]
  , cdUnique = False
  , cdKeywords = fromList [SpdrKeyword.Surge]
  }

mgRescueCaptives :: CardDef
mgRescueCaptives = (ultronDrones) {cdEncounterSet = Just MGSet.MutantGenesisCampaign, cdEncounterSetQuantity = Just 1, cdCardCode = "32173b", cdName = "Rescue Captives", cdBoostIcons = [MGBoost.Boost, MGBoost.Boost], cdTraits = fromList [MGTrait.Mission], cdHazards = 0, cdAcceleration = 0}

-- Mutant Genesis scenario definitions
mgProtectTheSenator :: CardDef
mgProtectTheSenator = (smEnvironmentDef "32065b" ("Protect the Senator") MGSet.Sabretooth 0)
  { cdTraits = fromList [MGTrait.Mission]
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }

mgSaveTheSchool :: CardDef
mgSaveTheSchool = (smEnvironmentDef "32130" ("Save the School") MGSet.MansionAttack 0)
  { cdTraits = fromList []
  , cdUnique = False
  , cdBoostIcons = []
  , cdKeywords = fromList []
  }
