module Marvel.AlterEgo.Cards where

import Marvel.CampaignPlayers.Cards qualified as CampaignPlayers
import Marvel.JessicaJones.Cards qualified as JL
import Marvel.FalconWinter.Cards qualified as FalconWinter

import Marvel.Ability.Types qualified as AospAbility ()
import Marvel.Aspect qualified as AospAspect ()
import Marvel.Boost qualified as AospBoost ()
import Marvel.Card.Def qualified as AospCard
import Marvel.Cost.Types qualified as AospCost
import Marvel.Criteria qualified as AospCriteria
import Marvel.EncounterSet qualified as AospSet ()
import Marvel.Keyword qualified as AospKeyword ()
import Marvel.Matchers qualified as AospMatch ()
import Marvel.Name qualified as AospName ()
import Marvel.Resource.Types qualified as AospResource ()
import Marvel.Trait qualified as AospTrait
import Marvel.Window.Types qualified as AospWindow ()

import Marvel.BlackPantherShuri.Cards qualified as BlackPantherShuriPack
import Marvel.Silk.Cards qualified as SilkPack
import Marvel.Window.Types qualified as NcWindow ()

import Marvel.Trait.Types qualified as NcTrait

import Marvel.Resource.Types qualified as NcResource ()

import Marvel.Name qualified as NcName ()

import Marvel.Matchers qualified as NcMatch ()

import Marvel.Keyword qualified as NcKeyword ()

import Marvel.EncounterSet qualified as NcSet ()

import Marvel.Criteria qualified as NcCriteria

import Marvel.Cost.Types qualified as NcCost

import Marvel.Card.Def qualified as NcCard ()

import Marvel.Boost qualified as NcBoost ()

import Marvel.Aspect qualified as NcAspect ()

import Marvel.Ability.Types qualified as NcAbility ()

import Marvel.Jubilee.Cards qualified as Jubilee

import Marvel.Iceman.Cards qualified as IcemanPack
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
import Marvel.Resource.Types qualified as MgResource ()
import Marvel.Trait qualified as MgTrait
import Marvel.Window.Types qualified as MgWindow ()

import Marvel.Trait qualified as AoaMT
import Marvel.Card.Def qualified as BpCard
import Marvel.Cost.Types qualified as BpCost
import Marvel.Criteria qualified as BpCriteria
import Marvel.Trait qualified as BpTrait

import Marvel.Ability.Types qualified as DpAbility ()
import Marvel.Aspect qualified as DpAspect ()
import Marvel.Boost qualified as DpBoost ()
import Marvel.Card.Def qualified as DpCard
import Marvel.Cost.Types qualified as DpCost
import Marvel.Criteria qualified as DpCriteria
import Marvel.EncounterSet qualified as DpSet ()
import Marvel.Keyword qualified as DpKeyword ()
import Marvel.Matchers qualified as DpMatch ()
import Marvel.Name qualified as DpName
import Marvel.Resource.Types qualified as DpResource ()
import Marvel.Trait qualified as DpTrait
import Marvel.Window.Types qualified as DpWindow ()

import Marvel.Ability.Types qualified as AnAbility ()
import Marvel.Aspect qualified as AnAspect ()
import Marvel.Boost qualified as AnBoost ()
import Marvel.Card.Def qualified as AnCard
import Marvel.Cost.Types qualified as AnCost
import Marvel.Criteria qualified as AnCriteria
import Marvel.EncounterSet qualified as AnSet ()
import Marvel.Keyword qualified as AnKeyword ()
import Marvel.Matchers qualified as AnMatch ()
import Marvel.Name qualified as AnName ()
import Marvel.Resource.Types qualified as AnResource ()
import Marvel.Trait qualified as AnTrait
import Marvel.Window.Types qualified as AnWindow ()

import Marvel.Ability.Types qualified as PsyAbility ()
import Marvel.Aspect qualified as PsyAspect ()
import Marvel.Boost qualified as PsyBoost ()
import Marvel.Card.Def qualified as PsyCard
import Marvel.Cost.Types qualified as PsyCost
import Marvel.Criteria qualified as PsyCriteria
import Marvel.EncounterSet qualified as PsySet ()
import Marvel.Keyword qualified as PsyKeyword ()
import Marvel.Matchers qualified as PsyMatch ()
import Marvel.Name qualified as PsyName ()
import Marvel.Resource.Types qualified as PsyResource ()
import Marvel.Trait qualified as PsyTrait
import Marvel.Window.Types qualified as PsyWindow ()

import Marvel.Trait qualified as NE
import Marvel.Ability.Types qualified as NEAbility ()

import Marvel.Boost qualified as NEB ()
import Marvel.EncounterSet qualified as NES ()
import Marvel.Aspect qualified as NEA ()
import Marvel.Resource.Types qualified as NER ()
import Marvel.Keyword qualified as NEK ()
import Marvel.Trait qualified as NET
import Marvel.WonderMan.Cards qualified as WM

import Marvel.Hercules.Cards qualified as HC

import Marvel.Prelude
import Marvel.Matchers ()
import Marvel.Resource ()
import Marvel.Keyword ()
import Marvel.EncounterSet ()
import Marvel.Boost ()

import Marvel.Cost.Types (Cost (NoCost))

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Trait

allAlterEgoCards :: [CardDef]
allAlterEgoCards =

    [ CampaignPlayers.cw56001b, CampaignPlayers.cw56029b, CampaignPlayers.fne60001b, CampaignPlayers.fne60037b
  , WM.wm58001b, HC.hc59001b
  , JL.jl61001b, JL.jl62001b
  , FalconWinter.fw53001b, FalconWinter.fw54001b
  , aosP50001b, aosP50034b
  , BlackPantherShuriPack.shuriAlterEgo, SilkPack.cindyMoon
  , nc48001b
  , Jubilee.jubilee47001b
  , bobbyDrake
  , erikLehnsherr
  , illyanaRasputin
  , lucasBishop
  , wadeWilson
  , lauraKinney
  , warrenWorthingtonIII
  , betsyBraddock
  , cable40001b
  , neenaThurman
  , annaMarie
  , remyLeBeau
  , ororoMunroe
  , scottSummers
  , logan
  , peterParker
    , carolDanvers
    , jenniferWalters
    , tonyStark
    , tChalla
    , steveRogers
    , kamalaKhan
    , natashaRomanoff
    , odinson
    , stephenStrange
    , bruceBanner
    -- The Green Goblin

    -- The Wrecking Crew

    -- The Rise of Red Skull
    , clintBarton
    , jessicaDrew

    -- The Once and Future Kang

    -- Ant-Man
    , scottLang

    -- Wasp
    , nadiaVanDyne

    -- Quicksilver
    , pietroMaximoff

    -- Scarlet Witch
    , wandaMaximoff

    -- Star-Lord
    , peterQuill

    -- Gamora
    , gamoraAlterEgo

    -- Groot
    , grootAlterEgo

    -- Rocket Raccoon
    , rocketRaccoonAlterEgo
    -- Drax
    , draxAlterEgo
    -- Venom
    , flashThompson
    -- Spectrum
    , monicaRambeau
    -- Adam Warlock
    , adamWarlockAlterEgo
    , nebulaAlterEgo
    , jamesRhodes
    , brunnhilde
    , visionAlterEgo
  , gwenStacy
  , milesMorales
  , samAlexander
  , ririWilliamsVersion1
  , ririWilliamsVersion2
  , ririWilliamsVersion3
  , peterPorker
  , peniParker
  , piotrRasputin
  , kittyPryde
  , jeanGrey
  ]

allAlterEgosMap :: HashMap CardCode CardDef
allAlterEgosMap = fromList $ map (toFst toCardCode) allAlterEgoCards

alterEgo :: CardCode -> Name -> [Trait] -> CardDef
alterEgo cardCode name traits = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
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

peterParker :: CardDef
peterParker = alterEgo "01001b" "Peter Parker" [Genius]

carolDanvers :: CardDef
carolDanvers = alterEgo "01010b" "Carol Danvers" [Shield, Soldier]

jenniferWalters :: CardDef
jenniferWalters = alterEgo "01019b" "Jennifer Walters" [Attorney, Gamma]

tonyStark :: CardDef
tonyStark = alterEgo "01029b" "Tony Stark" [Genius]

tChalla :: CardDef
tChalla = alterEgo "01040b" "T'Challa" [King, Wakanda]

steveRogers :: CardDef
steveRogers = alterEgo "03001b" "Steve Rogers" [Shield, Soldier]

kamalaKhan :: CardDef
kamalaKhan = alterEgo "05001b" "Kamala Khan" [Inhuman]
bruceBanner :: CardDef
bruceBanner = alterEgo "10001b" "Bruce Banner" [Genius, Scientist]

natashaRomanoff :: CardDef
natashaRomanoff = alterEgo "08001b" "Natasha Romanoff" [Shield, Spy]

odinson :: CardDef
odinson = alterEgo "06001b" "Odinson" [Asgard]

stephenStrange :: CardDef
stephenStrange = alterEgo "09001b" "Stephen Strange" [Mystic]

clintBarton :: CardDef
clintBarton = alterEgo "04001b" "Clint Barton" [Shield]

jessicaDrew :: CardDef
jessicaDrew = alterEgo "04031b" "Jessica Drew" [Shield, Spy]

scottLang :: CardDef
scottLang = alterEgo "12001b" "Scott Lang" [Civilian]

nadiaVanDyne :: CardDef
nadiaVanDyne = alterEgo "13001b" "Nadia Van Dyne" [Genius]

pietroMaximoff :: CardDef
pietroMaximoff = alterEgo "14001b" "Pietro Maximoff" [Civilian]

-- Scarlet Witch

wandaMaximoff :: CardDef
wandaMaximoff = alterEgo "15001b" "Wanda Maximoff" [Mystic]

-- Star-Lord

peterQuill :: CardDef
peterQuill = alterEgo "17001b" "Peter Quill" [Outlaw]

-- Gamora

gamoraAlterEgo :: CardDef
gamoraAlterEgo = alterEgo "18001b" "Gamora" [Outlaw]

-- Groot

grootAlterEgo :: CardDef
grootAlterEgo = alterEgo "16001b" "Groot" [Outlaw]

-- Rocket Raccoon

rocketRaccoonAlterEgo :: CardDef
rocketRaccoonAlterEgo = alterEgo "16029b" "Rocket Raccoon" [Genius, Outlaw]
-- Drax

draxAlterEgo :: CardDef
draxAlterEgo = alterEgo "19001b" "Drax" [Outlaw]

-- Venom

flashThompson :: CardDef
flashThompson = alterEgo "20001b" "Flash Thompson" [Soldier]

-- Spectrum

monicaRambeau :: CardDef
monicaRambeau = alterEgo "21001b" "Monica Rambeau" [Civilian]

-- Adam Warlock

adamWarlockAlterEgo :: CardDef
adamWarlockAlterEgo = alterEgo "21031b" "Adam Warlock" [Mystic]

-- Nebula

nebulaAlterEgo :: CardDef
nebulaAlterEgo = deckLimited 1 $ (alterEgo "22001b" "Nebula" [Outlaw])
    { cdUnique = True
    }

-- War Machine

jamesRhodes :: CardDef
jamesRhodes = deckLimited 1 $ (alterEgo "23001b" "James Rhodes" [Soldier, Shield])
    { cdUnique = True
    }

-- Valkyrie

brunnhilde :: CardDef
brunnhilde = deckLimited 1 $ (alterEgo "25001b" "Brunnhilde" [Asgard])
    { cdUnique = True
    }

-- Vision

visionAlterEgo :: CardDef
visionAlterEgo = deckLimited 1 $ (alterEgo "26001b" "Vision" [Android])
    { cdUnique = True
    }


-- Sinister Motives

gwenStacy :: CardDef
gwenStacy = alterEgo "27001b" "Gwen Stacy" [Civilian]

milesMorales :: CardDef
milesMorales = alterEgo "27030b" "Miles Morales" [Civilian]

-- Nova

samAlexander :: CardDef
samAlexander = (alterEgo "28001b" "Sam Alexander" [Civilian])
  { cdDeckLimit = Just 1
  , cdUnique = True
  }


-- Ironheart

ririWilliamsVersion1 :: CardDef
ririWilliamsVersion1 = alterEgo "29001b" "Riri Williams" [Genius]

ririWilliamsVersion2 :: CardDef
ririWilliamsVersion2 = alterEgo "29002b" "Riri Williams" [Genius]

ririWilliamsVersion3 :: CardDef
ririWilliamsVersion3 = alterEgo "29003b" "Riri Williams" [Genius]


-- Spider-Ham

peterPorker :: CardDef
peterPorker = alterEgo "30001b" "Peter Porker" [Cartoon, Civilian]


-- SP//dr

peniParker :: CardDef
peniParker = (alterEgo "31002a" "Peni Parker" [Civilian])
  { cdUnique = True
  }

-- Mutant Genesis

piotrRasputin :: CardDef
piotrRasputin = (alterEgo "32001b" "Piotr Rasputin" [Mutant])
  { cdUnique = True
  }

-- Mutant Genesis

kittyPryde :: CardDef
kittyPryde = (alterEgo "32030b" "Kitty Pryde" [Mutant])
  { cdUnique = True
  }

-- Wolverine

logan :: CardDef
logan = (alterEgo "35001b" "Logan" [Mutant]) {cdUnique = True}


-- Phoenix

jeanGrey :: CardDef
jeanGrey = alterEgo "34001b" "Jean Grey" [Mutant, Psionic]


-- Cyclops

scottSummers :: CardDef
scottSummers = (alterEgo "33001b" "Scott Summers" [Mutant]) { cdDeckLimit = Just 1, cdUnique = True }


-- Storm

ororoMunroe :: CardDef
ororoMunroe = (alterEgo "36001b" "Ororo Munroe" [Mutant]) { cdUnique = True }

-- Gambit

remyLeBeau :: CardDef
remyLeBeau = (alterEgo "37001b" "Remy LeBeau" [Mutant, Thief]) { cdUnique = True }


-- Rogue

annaMarie :: CardDef
annaMarie = (alterEgo "38001b" "Anna Marie" [Mutant]) { cdUnique = True }

-- NeXt Evolution: Domino
neenaThurman :: CardDef
neenaThurman = CardDef
  { cdCardCode = "40037b", cdName = "Neena Thurman", cdCost = Nothing
  , cdTraits = fromList [NET.Mutant], cdKeywords = fromList []
  , cdCardType = AlterEgoType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }

-- NeXt Evolution: Cable

cable40001b :: CardDef
cable40001b = (alterEgo "40001b" "Nathan Summers" [NE.Mutant, NE.Soldier]) { cdUnique = True, cdDeckLimit = Just 1 }

-- Psylocke

betsyBraddock :: CardDef
betsyBraddock = CardDef
  { cdCardCode = "41001b", cdName = "Betsy Braddock", cdCost = Nothing
  , cdTraits = fromList [PsyTrait.Mutant, PsyTrait.Psionic], cdKeywords = fromList []
  , cdCardType = PsyCard.AlterEgoType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Nothing, cdCriteria = PsyCriteria.NoCriteria
  , cdResources = [], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing
  , cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Just 1, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = PsyCost.NoCost
  }

-- Angel

warrenWorthingtonIII :: CardDef
warrenWorthingtonIII = CardDef
  { cdCardCode = "42001b"
  , cdName = "Warren Worthington III"
  , cdCost = Nothing
  , cdTraits = fromList [AnTrait.Mutant]
  , cdKeywords = fromList []
  , cdCardType = AnCard.AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

-- X-23

lauraKinney :: CardDef
lauraKinney = CardDef
  { cdCardCode = "43001b"
  , cdName = "Laura Kinney"
  , cdCost = Nothing
  , cdTraits = fromList [AnTrait.Mutant]
  , cdKeywords = fromList []
  , cdCardType = AnCard.AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AnCriteria.NoCriteria
  , cdResources = []
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AnCost.NoCost
  }

-- Deadpool

wadeWilson :: CardDef
wadeWilson = DpCard.CardDef
  { cdCardCode = "44001b"
  , cdName = DpName.Name "Wade Wilson" (Nothing)
  , cdCost = Nothing
  , cdTraits = fromList [DpTrait.Mercenary, DpTrait.Mutant]
  , cdKeywords = fromList []
  , cdCardType = DpCard.AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = DpCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
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
  , cdAbilityCost = DpCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Bishop

lucasBishop :: CardDef
lucasBishop = CardDef
  { cdCardCode = "45001b"
  , cdName = "Lucas Bishop"
  , cdCost = Nothing
  , cdTraits = fromList [BpTrait.Mutant, BpTrait.Temporal]
  , cdKeywords = fromList []
  , cdCardType = BpCard.AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = BpCriteria.NoCriteria
  , cdResources = []
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = BpCost.NoCost
  }

-- Age of Apocalypse: Magik
illyanaRasputin :: CardDef
illyanaRasputin = CardDef
  { cdCardCode = "45030b"
  , cdName = "Illyana Rasputin"
  , cdCost = Nothing
  , cdTraits = fromList [AoaMT.Mutant, AoaMT.Mystic]
  , cdKeywords = fromList []
  , cdCardType = AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = NoCost
  }

-- Magneto

erikLehnsherr :: CardDef
erikLehnsherr = CardDef
  { cdCardCode = "49001b"
  , cdName = "Erik Lehnsherr"
  , cdCost = Nothing
  , cdTraits = fromList [MgTrait.Mutant]
  , cdKeywords = fromList []
  , cdCardType = MgCard.AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = MgCriteria.NoCriteria
  , cdResources = []
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = MgCost.NoCost
  }

-- Iceman
bobbyDrake :: CardDef
bobbyDrake = IcemanPack.bobbyDrake

-- Nightcrawler
nc48001b :: CardDef
nc48001b = CardDef
  { cdCardCode = "48001b"
  , cdName = "Kurt Wagner"
  , cdCost = Nothing
  , cdTraits = fromList [NcTrait.Mutant]
  , cdKeywords = fromList []
  , cdCardType = AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NcCriteria.NoCriteria
  , cdResources = []
  , cdBoostIcons = []
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
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
  , cdAbilityCost = NcCost.NoCost
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

-- Agents of Shield players

aosP50001b :: CardDef
aosP50001b = CardDef
  { cdCardCode = "50001b"
  , cdName = "Maria Hill"
  , cdCost = Nothing
  , cdTraits = fromList [AospTrait.Shield, AospTrait.Spy]
  , cdKeywords = fromList []
  , cdCardType = AospCard.AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = []
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }

aosP50034b :: CardDef
aosP50034b = CardDef
  { cdCardCode = "50034b"
  , cdName = "Nick Fury"
  , cdCost = Nothing
  , cdTraits = fromList [AospTrait.Shield, AospTrait.Soldier, AospTrait.Spy]
  , cdKeywords = fromList []
  , cdCardType = AospCard.AlterEgoType
  , cdAbilityType = Nothing
  , cdAbilitySubTypes = []
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = AospCriteria.NoCriteria
  , cdResources = []
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
  , cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  , cdAbilityCost = AospCost.NoCost
  }
