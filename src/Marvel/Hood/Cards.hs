module Marvel.Hood.Cards where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Name

hoodCardDef :: CardCode -> Name -> EncounterSet -> CardType -> Natural -> CardDef
hoodCardDef code name set cardType quantity = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = cardType
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
