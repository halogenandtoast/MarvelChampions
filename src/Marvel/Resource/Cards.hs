module Marvel.Resource.Cards where

import Marvel.Prelude

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.EncounterSet qualified as EncounterSet
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

  -- Rocket Raccoon
  , salvage

  -- Spectrum
  , bandTogether

  -- Adam Warlock
  , audacity
  , determination
  , innovation
  , preservation

  -- The Mad Titan's Shadow: the campaign set
  , shawarma
  , connectionToTheWorldmind
  , everydayHero
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

energyAbsorption :: CardDef
energyAbsorption =
  identityResource "01014" "Energy Absorption" [] [Energy, Energy, Energy]

vibranium :: CardDef
vibranium = identityResource "01044" "Vibranium" [] [Wild, Wild]

thePowerOfAggression :: CardDef
thePowerOfAggression = deckLimited 2 $ reprintedAs ["06016", "10017", "13015", "25022"] $
  resource "01055" "The Power of Aggression" [] [Wild] Aggression
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Aggression, Wild)]

thePowerOfJustice :: CardDef
thePowerOfJustice = deckLimited 2 $ reprintedAs ["08014", "15016", "20014", "22017"] $
  resource "01062" "The Power of Justice" [] [Wild] Justice
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Justice, Wild)]

thePowerOfLeadership :: CardDef
thePowerOfLeadership = reprintedAs ["03018", "04019", "17018"] $
  resource "01072" "The Power of Leadership" [] [Wild] Leadership
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Leadership, Wild)]

thePowerOfProtection :: CardDef
thePowerOfProtection = reprintedAs ["05016", "09017", "16015"] $
  resource "01079" "The Power of Protection" [] [Wild] Protection
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Protection, Wild)]

energy :: CardDef
energy = deckLimited 1 $ reprintedAs ["27020", "27051", "03021", "05019", "06022", "08020", "09022", "10020", "04023", "04050", "12021", "13021", "14019", "15020", "18021", "16021", "16049", "19022", "20017", "21023", "22024", "23025", "25025", "26025"] $ basicResource "01088" "Energy" [] [Energy, Energy]

genius :: CardDef
genius = deckLimited 1 $ reprintedAs ["27021", "27052", "03022", "05020", "06023", "08021", "09023", "10021", "04024", "04051", "12022", "13022", "14020", "15021", "18022", "16022", "16050", "19023", "20018", "21024", "22025", "23026", "25026", "26026"] $ basicResource "01089" "Genius" [] [Mental, Mental]

strength :: CardDef
strength = deckLimited 1 $ reprintedAs ["27022", "27053", "03023", "05021", "06024", "08022", "09024", "10022", "04025", "04052", "12023", "13023", "14021", "15022", "18023", "16023", "16051", "19024", "20019", "21025", "22026", "23027", "25027", "26027"] $ basicResource "01090" "Strength" [] [Physical, Physical]

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

-- Rocket Raccoon

-- | Its "after you spend this card" is 'Marvel.Resource.Resources.afterSpent'.
salvage :: CardDef
salvage = identityResource "16033" "Salvage" [] [Mental, Wild]

-- Spectrum

-- | "This card generates [wild] for each ally you control (to a maximum of 3)": no printed
-- resource.
bandTogether :: CardDef
bandTogether = resource "21018" "Band Together" [] [] Leadership & resourcesL .~ [(ForEachAllyControlled 3, Wild)]

-- Adam Warlock (each Max 1 per deck)

audacity :: CardDef
audacity = deckLimited 1 $ reprintedAs ["25021"] $ resource "21046" "Audacity" [] [Wild] Aggression

determination :: CardDef
determination = deckLimited 1 $ reprintedAs ["22016"] $ resource "21052" "Determination" [] [Wild] Justice

innovation :: CardDef
innovation = deckLimited 1 $ reprintedAs ["23021"] $ resource "21058" "Innovation" [] [Wild] Leadership

preservation :: CardDef
preservation = deckLimited 1 $ reprintedAs ["26021"] $ resource "21064" "Preservation" [] [Wild] Protection

-- The Mad Titan's Shadow: the campaign set

-- | Basic, and campaign-only: printed "Max 1 per deck", though no deck brought to a table
-- may hold it; the campaign shuffles one copy into each player's deck.
shawarma :: CardDef
shawarma =
  (basicResource "21183" "Shawarma" [] [Energy, Physical, Mental])
    { cdEncounterSet = Just EncounterSet.MadTitansShadowCampaign
    , cdEncounterSetQuantity = Just 4
    }

-- Nova

connectionToTheWorldmind :: CardDef
connectionToTheWorldmind = (identityResource "28007" "Connection to the Worldmind" [] [Wild])
  { cdDeckLimit = Just 2
  , cdDoesNotCountTowardHandSize = True
  }

everydayHero :: CardDef
everydayHero = (basicResource "28019" "Everyday Hero" [] [Wild])
  { cdDeckLimit = Just 3
  , cdCanPayForOthersWithTrait = Just Civilian
  }
