module Marvel.Resource.Cards where

import Marvel.Ability.Types qualified as AospAbility ()
import Marvel.Aspect qualified as AospAspect
import Marvel.Boost qualified as AospBoost ()
import Marvel.Card.Def qualified as AospCard
import Marvel.Cost.Types qualified as AospCost
import Marvel.Criteria qualified as AospCriteria
import Marvel.EncounterSet qualified as AospSet ()
import Marvel.Keyword qualified as AospKeyword ()
import Marvel.Matchers qualified as AospMatch ()
import Marvel.Name qualified as AospName ()
import Marvel.Resource.Types qualified as AospResource
import Marvel.Trait qualified as AospTrait ()
import Marvel.Window.Types qualified as AospWindow ()

import Marvel.BlackPantherShuri.Cards qualified as BlackPantherShuriPack
import Marvel.Jubilee.Cards qualified as Jubilee

import Marvel.Ability.Types qualified as MgAbility ()
import Marvel.Aspect qualified as MgAspect ()
import Marvel.Boost qualified as MgBoost ()
import Marvel.Card.Def qualified as MgCard
import Marvel.Cost.Types qualified as MgCost
import Marvel.Criteria qualified as MgCriteria
import Marvel.EncounterSet qualified as MgSet ()
import Marvel.Keyword qualified as MgKeyword ()
import Marvel.Matchers qualified as MgMatch ()
import Marvel.Name qualified as MgName ()
import Marvel.Resource.Types qualified as MgResource
import Marvel.Trait qualified as MgTrait
import Marvel.Window.Types qualified as MgWindow ()

import Marvel.Card.Def qualified as BpCard
import Marvel.Cost.Types qualified as BpCost
import Marvel.Criteria qualified as BpCriteria
import Marvel.Resource.Types qualified as BpResource
import Marvel.Trait qualified as BpTrait

import Marvel.Ability.Types qualified as DpAbility ()
import Marvel.Aspect qualified as DpAspect
import Marvel.Boost qualified as DpBoost ()
import Marvel.Card.Def qualified as DpCard
import Marvel.Cost.Types qualified as DpCost
import Marvel.Criteria qualified as DpCriteria
import Marvel.EncounterSet qualified as DpSet ()
import Marvel.Keyword qualified as DpKeyword ()
import Marvel.Matchers qualified as DpMatch ()
import Marvel.Name qualified as DpName
import Marvel.Resource.Types qualified as DpResource
import Marvel.Trait qualified as DpTrait ()
import Marvel.Window.Types qualified as DpWindow ()

import Marvel.Ability.Types qualified as AnAbility ()
import Marvel.Aspect qualified as AnAspect
import Marvel.Boost qualified as AnBoost ()
import Marvel.Card.Def qualified as AnCard
import Marvel.Cost.Types qualified as AnCost
import Marvel.Criteria qualified as AnCriteria
import Marvel.EncounterSet qualified as AnSet ()
import Marvel.Keyword qualified as AnKeyword ()
import Marvel.Matchers qualified as AnMatch ()
import Marvel.Name qualified as AnName ()
import Marvel.Resource.Types qualified as AnResource
import Marvel.Trait qualified as AnTrait
import Marvel.Window.Types qualified as AnWindow ()



import Marvel.Resource.Types qualified as NEResource

import Marvel.Boost qualified as NEBoost ()

import Marvel.GameValue.Types qualified as NEValue ()

import Marvel.EncounterSet qualified as NESet

import Marvel.Keyword qualified as NEKeyword ()

import Marvel.Trait qualified as NETrait ()

import Marvel.Aspect qualified as NEAspect

import Marvel.Boost qualified as NEB ()
import Marvel.EncounterSet qualified as NES ()
import Marvel.Aspect qualified as NEA
import Marvel.Resource.Types qualified as NER
import Marvel.Keyword qualified as NEK ()
import Marvel.Trait qualified as NET ()
import Marvel.Trait qualified as NE
import Marvel.Resource qualified as NE
import Marvel.Card.Def qualified as NE
import Marvel.Ability.Types qualified as NEAbility ()

import Marvel.Matchers ()
import Marvel.Boost ()
import Marvel.Keyword ()
import Marvel.EncounterSet ()
import Marvel.Cost.Types
import Marvel.Window ()
import Marvel.WonderMan.Cards qualified as WM


import Marvel.Prelude


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
  [ WM.wm58006
  , aosP50007, aosP50014
  , BlackPantherShuriPack.vibraniumShuri
  , Jubilee.jubilee47010a, Jubilee.jubilee47010b, Jubilee.jubilee47010c
  , masterOfMagnetism
  , storedEnergy
  , montage, selfConfidence, selfControl, selfPreservation
  , avianAnatomy, thePowerOfFlight
  , ne40196
  , diggingDeep
  , jackpot
  , cable40028
  , molecularAcceleration
  , effectiveLeadership
  , energyAbsorption
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
  , strokeOfGenius
  , defensiveEnergy
  , aggressiveEnergy
  , passionForJustice
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
thePowerOfAggression = deckLimited 2 $ reprintedAs ["45047", "06016", "10017", "13015", "25022", "28015"] $
  resource "01055" "The Power of Aggression" [] [Wild] Aggression
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Aggression, Wild)]

thePowerOfJustice :: CardDef
thePowerOfJustice = deckLimited 2 $ reprintedAs ["47017", "08014", "15016", "20014", "22017"] $
  resource "01062" "The Power of Justice" [] [Wild] Justice
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Justice, Wild)]

thePowerOfLeadership :: CardDef
thePowerOfLeadership = reprintedAs ["45019", "03018", "04019", "17018", "29021", "62017"] $
  resource "01072" "The Power of Leadership" [] [Wild] Leadership
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Leadership, Wild)]

thePowerOfProtection :: CardDef
thePowerOfProtection = reprintedAs ["48019", "05016", "09017", "16015"] $
  resource "01079" "The Power of Protection" [] [Wild] Protection
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Protection, Wild)]

energy :: CardDef
energy = reprintedAs ["59029"] $ reprintedAs ["58023"] $ reprintedAs ["60057"] $  reprintedAs ["56020"] $  reprintedAs ["54024", "53025", "50025", "52025", "51027", "48023", "49024", "45022", "43022", "40061", "38021", "37022", "36027", "33024", "34025", "35024", "32022", "32052", "27020", "27051", "03021", "05019", "06022", "08020", "09022", "10020", "04023", "04050", "12021", "13021", "14019", "15020", "18021", "16021", "16049", "19022", "20017", "21023", "22024", "23025", "25025", "26025", "62025"] $ deckLimited 1 $ basicResource "01088" "Energy" [] [Energy, Energy]

genius :: CardDef
genius = reprintedAs ["59030"] $ reprintedAs ["60058"] $  reprintedAs ["56021"] $  reprintedAs ["54025", "53026", "50026", "52026", "51028", "48024", "49025", "45023", "43023", "40062", "38022", "37023", "36028", "33025", "34026", "35025", "32023", "32053", "27021", "27052", "03022", "05020", "06023", "08021", "09023", "10021", "04024", "04051", "12022", "13022", "14020", "15021", "18022", "16022", "16050", "19023", "20018", "21024", "22025", "23026", "25026", "26026", "62026"] $ deckLimited 1 $ basicResource "01089" "Genius" [] [Mental, Mental]

strength :: CardDef
strength = reprintedAs ["59031"] $ reprintedAs ["60059"] $  reprintedAs ["56022"] $  reprintedAs ["54026", "53027", "50027", "52027", "51029", "48025", "49026", "45024", "43024", "40063", "38023", "37024", "36029", "33026", "34027", "35026", "32024", "32054", "27022", "27053", "03023", "05021", "06024", "08022", "09024", "10022", "04025", "04052", "12023", "13023", "14021", "15022", "18023", "16023", "16051", "19024", "20019", "21025", "22026", "23027", "25027", "26027", "62027"] $ deckLimited 1 $ basicResource "01090" "Strength" [] [Physical, Physical]

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
  reprintedAs ["46023"] (basicResource "13024" "The Power in All of Us" [] [Wild]
    & resourcesL
    <>~ [(ResourceForCardsMatching $ CardWithAspect Neutral, Wild)])

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
audacity = reprintedAs ["56018"] $  deckLimited 1 $ reprintedAs ["25021"] $ resource "21046" "Audacity" [] [Wild] Aggression

determination :: CardDef
determination = deckLimited 1 $ reprintedAs ["22016", "61022"] $ resource "21052" "Determination" [] [Wild] Justice

innovation :: CardDef
innovation = deckLimited 1 $ reprintedAs ["23021", "62016"] $ resource "21058" "Innovation" [] [Wild] Leadership

preservation :: CardDef
preservation = reprintedAs ["56051"] $  deckLimited 1 $ reprintedAs ["26021"] $ resource "21064" "Preservation" [] [Wild] Protection

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


-- Ironheart

strokeOfGenius :: CardDef
strokeOfGenius = (identityResource "29009" "Stroke of Genius" [] [Mental])
  { cdDeckLimit = Just 2
  , cdUnique = False
  }

-- Mutant Genesis

defensiveEnergy :: CardDef
defensiveEnergy = reprintedAs ["38017"] $ (resource "32018" "Defensive Energy" [] [Wild] Protection)
  { cdDeckLimit = Just 2
  }

-- Mutant Genesis

aggressiveEnergy :: CardDef
aggressiveEnergy = reprintedAs ["35020"] $ (resource "32047" "Aggressive Energy" [] [Wild] Aggression)
  { cdDeckLimit = Just 2
  }


-- Phoenix

passionForJustice :: CardDef
passionForJustice = reprintedAs ["37016"] $ deckLimited 2 $ resource "34020" "Passion for Justice" [] [Wild] Justice


-- Cyclops

effectiveLeadership :: CardDef
effectiveLeadership = reprintedAs ["36021"] $ (resource "33018" "Effective Leadership" [] [Wild] Leadership) { cdDeckLimit = Just 2 }

-- Gambit

molecularAcceleration :: CardDef
molecularAcceleration = (identityResource "37010" "Molecular Acceleration" [] [Energy, Physical]) { cdDeckLimit = Just 2 }

-- NeXt Evolution: Cable

cable40028 :: CardDef
cable40028 = reprintedAs ["41021"] $ (basicResource "40028" "The Power of the Mind" [] [NE.Mental]) { cdUnique = False, cdDeckLimit = Just 3, cdResources = [(NE.PrintedResource, NE.Mental), (NE.ResourceForCardsMatching (NE.CardWithTrait NE.Psionic), NE.Mental)] }

-- NeXt Evolution: Domino
jackpot :: CardDef
jackpot = CardDef
  { cdCardCode = "40043", cdName = "Jackpot!", cdCost = Nothing
  , cdTraits = fromList [], cdKeywords = fromList []
  , cdCardType = ResourceType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Energy), (PrintedResource, NER.Physical), (PrintedResource, NER.Mental)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Domino
diggingDeep :: CardDef
diggingDeep = CardDef
  { cdCardCode = "40060", cdName = "Digging Deep", cdCost = Nothing
  , cdTraits = fromList [], cdKeywords = fromList []
  , cdCardType = ResourceType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = False, cdAspect = Just NEA.Neutral, cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NER.Wild)], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 3, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution campaign and scenarios

ne40196 :: CardDef
ne40196 = CardDef
  { cdCardCode = "40196"
  , cdName = "Pouches"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just NEAspect.Neutral
  , cdCriteria = NoCriteria
  , cdResources = [(PrintedResource, NEResource.Wild), (PrintedResource, NEResource.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Just NESet.NextEvolutionCampaign
  , cdEncounterSetQuantity = Just 4
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
  , cdAbilityCost = NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Angel

avianAnatomy :: CardDef
avianAnatomy = CardDef
  { cdCardCode = "42008"
  , cdName = "Avian Anatomy"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AnCard.ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

thePowerOfFlight :: CardDef
thePowerOfFlight = reprintedAs ["53028"] $ CardDef
  { cdCardCode = "42022"
  , cdName = "The Power of Flight"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AnCard.ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just AnAspect.Neutral
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = [(AnCard.PrintedResource, AnResource.Energy), (AnCard.ResourceForCardsMatching (AnCard.CardWithTrait AnTrait.Aerial), AnResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

-- Deadpool

montage :: CardDef
montage = DpCard.CardDef
  { cdCardCode = "44007"
  , cdName = DpName.Name "Montage" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Wild), (DpCard.ForEachMainSchemeAccelerationToken 3, DpResource.Wild)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

selfConfidence :: CardDef
selfConfidence = DpCard.CardDef
  { cdCardCode = "44025"
  , cdName = DpName.Name "Self Confidence" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Physical), (DpCard.ResourceIfIdentityDamageLessThan 5, DpResource.Physical), (DpCard.ResourceIfIdentityDamageLessThan 1, DpResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

selfControl :: CardDef
selfControl = DpCard.CardDef
  { cdCardCode = "44026"
  , cdName = DpName.Name "Self Control" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Mental), (DpCard.ResourceIfIdentityDamageLessThan 5, DpResource.Mental), (DpCard.ResourceIfIdentityDamageLessThan 1, DpResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

selfPreservation :: CardDef
selfPreservation = DpCard.CardDef
  { cdCardCode = "44027"
  , cdName = DpName.Name "Self Preservation" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = DpCard.ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just DpAspect.Pool
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = [(DpCard.PrintedResource, DpResource.Energy), (DpCard.ResourceIfIdentityDamageLessThan 5, DpResource.Energy), (DpCard.ResourceIfIdentityDamageLessThan 1, DpResource.Energy)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 1
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Bishop

storedEnergy :: CardDef
storedEnergy = CardDef
  { cdCardCode = "45010"
  , cdName = "Stored Energy"
  , cdCost = Nothing
  , cdTraits = fromList [BpTrait.Temporal]
  , cdKeywords = fromList []
  , cdCardType = BpCard.ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = BpCriteria.NoCriteria
  , cdResources = [(BpCard.PrintedResource, BpResource.Energy), (BpCard.PrintedResource, BpResource.Physical)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = BpCost.NoCost
  }

-- Magneto

masterOfMagnetism :: CardDef
masterOfMagnetism = CardDef
  { cdCardCode = "49011"
  , cdName = "Master of Magnetism"
  , cdCost = Nothing
  , cdTraits = fromList [MgTrait.Magnetic]
  , cdKeywords = fromList []
  , cdCardType = MgCard.ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = [(MgCard.PrintedResource, MgResource.Energy), (MgCard.PrintedResource, MgResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

-- Agents of Shield players

aosP50007 :: CardDef
aosP50007 = CardDef
  { cdCardCode = "50007"
  , cdName = "Special Funding"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AospCard.ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Physical), (AospCard.PrintedResource, AospResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 2
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }

aosP50014 :: CardDef
aosP50014 = CardDef
  { cdCardCode = "50014"
  , cdName = "Organizational Support"
  , cdCost = Nothing
  , cdTraits = fromList []
  , cdKeywords = fromList []
  , cdCardType = AospCard.ResourceType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = False
  , cdAspect = Just AospAspect.Leadership
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = [(AospCard.PrintedResource, AospResource.Mental)]
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing
  , cdActionCriteria = Nothing
  , cdHazards = 0
  , cdAcceleration = 0
  , cdAmplify = 0
  , cdLimit = Nothing
  , cdDeckLimit = Just 3
  , cdTeamUp = Nothing
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }
