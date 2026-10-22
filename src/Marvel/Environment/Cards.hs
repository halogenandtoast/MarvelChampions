module Marvel.Environment.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet

allEnvironments :: HashMap CardCode CardDef
allEnvironments = fromList $ map (toFst toCardCode)
  [ ultronDrones
  -- The Green Goblin
  , criminalEnterprise
  , stateOfMadness

  -- The Wrecking Crew

  -- The Rise of Red Skull

  -- The Once and Future Kang

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
    , cdAbilitySubType = Nothing
    , cdUnique = False
    , cdAspect = Nothing
    , cdEncounterSet = Just Ultron
    , cdEncounterSetQuantity = Just 0
    , cdCriteria = NoCriteria
    , cdResources = []
    , cdResponseWindow = Nothing
    , cdBoostIcons = []
    , cdHazards = 0
    , cdAcceleration = 0
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
