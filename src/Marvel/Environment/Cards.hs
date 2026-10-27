module Marvel.Environment.Cards where

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
  [ ultronDrones
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

  -- The Galaxy's Most Wanted: Escape the Museum
  , libraryLabyrinth
  , museumShip
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
    , cdAlternateCardCodes = []
    , cdPlayableFromDiscard = False
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
