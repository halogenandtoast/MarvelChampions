module Marvel.Resource.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Resource
import Marvel.Trait

allResources :: HashMap CardCode CardDef
allResources = fromList $ concatMap
  toCardCodePairs
  [ energyAbsorption
  , vibranium
  , thePowerOfAggression
  , thePowerOfJustice
  , thePowerOfLeadership
  , thePowerOfProtection
  , energy
  , genius
  , strength
  , limitlessStrength
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull

  -- The Once and Future Kang


  -- Ant-Man
  , pymParticles

  -- Wasp
  , thePowerInAllOfUs
  ]

resource :: CardCode -> Name -> [Trait] -> [Resource] -> Aspect -> CardDef
resource code name traits resources aspect =
  baseResource code name traits resources (Just aspect)

identityResource :: CardCode -> Name -> [Trait] -> [Resource] -> CardDef
identityResource code name traits resources =
  baseResource code name traits resources Nothing

basicResource :: CardCode -> Name -> [Trait] -> [Resource] -> CardDef
basicResource code name traits resources =
  baseResource code name traits resources (Just Neutral)

baseResource
  :: CardCode -> Name -> [Trait] -> [Resource] -> Maybe Aspect -> CardDef
baseResource code name traits resources mAspect = CardDef
  { cdCardCode = code
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdCriteria = NoCriteria
  , cdAspect = mAspect
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResources = map (PrintedResource, ) resources
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  , cdLimit = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = NoCost
  }

energyAbsorption :: CardDef
energyAbsorption =
  identityResource "01014" "Energy Absorption" [] [Energy, Energy, Energy]

vibranium :: CardDef
vibranium = identityResource "01044" "Vibranium" [] [Wild, Wild]

thePowerOfAggression :: CardDef
thePowerOfAggression = reprintedAs ["06016", "10017", "13015"] $
  resource "01055" "The Power of Aggression" [] [Wild] Aggression
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Aggression, Wild)]

thePowerOfJustice :: CardDef
thePowerOfJustice = reprintedAs ["08014", "15016", "20014"] $
  resource "01062" "The Power of Justice" [] [Wild] Justice
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Justice, Wild)]

thePowerOfLeadership :: CardDef
thePowerOfLeadership = reprintedAs ["03018", "04019", "17018"] $
  resource "01072" "The Power of Leadership" [] [Wild] Leadership
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Leadership, Wild)]

thePowerOfProtection :: CardDef
thePowerOfProtection = reprintedAs ["05016", "09017"] $
  resource "01079" "The Power of Protection" [] [Wild] Protection
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Protection, Wild)]

energy :: CardDef
energy = reprintedAs ["03021", "05019", "06022", "08020", "09022", "10020", "04023", "04050", "12021", "13021", "14019", "15020", "18021", "19022", "20017"] $ basicResource "01088" "Energy" [] [Energy, Energy]

genius :: CardDef
genius = reprintedAs ["03022", "05020", "06023", "08021", "09023", "10021", "04024", "04051", "12022", "13022", "14020", "15021", "18022", "19023", "20018"] $ basicResource "01089" "Genius" [] [Mental, Mental]

strength :: CardDef
strength = reprintedAs ["03023", "05021", "06024", "08022", "09024", "10022", "04025", "04052", "12023", "13023", "14021", "15022", "18023", "19024", "20019"] $ basicResource "01090" "Strength" [] [Physical, Physical]

-- | "Spend this card only in hero form."
limitlessStrength :: CardDef
limitlessStrength =
  (identityResource "10007" "Limitless Strength" [] [Physical, Physical, Physical])
    { cdCriteria = InHeroForm
    }

-- Ant-Man

-- | Its "after you spend this card" is 'Marvel.Resource.Resources.afterSpent'.
pymParticles :: CardDef
pymParticles = reprintedAs ["13007"] $ identityResource "12006" "Pym Particles" [] [Wild]

-- Wasp

thePowerInAllOfUs :: CardDef
thePowerInAllOfUs =
  basicResource "13024" "The Power in All of Us" [] [Wild]
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Neutral, Wild)]
