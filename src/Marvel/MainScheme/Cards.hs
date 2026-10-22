module Marvel.MainScheme.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Name

allMainSchemes :: HashMap CardCode CardDef
allMainSchemes =
  fromList $ map (toFst toCardCode) [theBreakIn, undergroundDistribution, secretRendezvous, theCrimsonCowl, assaultOnNorad, countdownToOblivion]

mainScheme :: CardCode -> Name -> EncounterSet -> CardDef
mainScheme code name encounterSet = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = mempty
  , cdKeywords = mempty
  , cdCardType = MainSchemeType
  , cdAbilityType = Nothing
  , cdAbilitySubType = Nothing
  , cdUnique = False
  , cdAspect = Nothing
  , cdEncounterSet = Just encounterSet
  , cdEncounterSetQuantity = Just 1
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

theBreakIn :: CardDef
theBreakIn = mainScheme "01097" "The Break-In!" Rhino

undergroundDistribution :: CardDef
undergroundDistribution = mainScheme "01116" "Underground Distribution" Klaw

secretRendezvous :: CardDef
secretRendezvous = mainScheme "01117" "Secret Rendezvous" Klaw

theCrimsonCowl :: CardDef
theCrimsonCowl = mainScheme "01137" "The Crimson Cowl" Ultron

assaultOnNorad :: CardDef
assaultOnNorad = mainScheme "01138" "Assault on NORAD" Ultron

countdownToOblivion :: CardDef
countdownToOblivion = mainScheme "01139" "Countdown to Oblivion" Ultron
