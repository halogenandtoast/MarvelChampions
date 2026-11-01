module Marvel.Hero.Cards where

import Marvel.Boost qualified as NEB
import Marvel.EncounterSet qualified as NES
import Marvel.Aspect qualified as NEA
import Marvel.Resource.Types qualified as NER
import Marvel.Keyword qualified as NEK
import Marvel.Trait qualified as NET
import Marvel.Trait qualified as NE
import Marvel.Resource qualified as NE
import Marvel.Keyword qualified as NE
import Marvel.Boost qualified as NE
import Marvel.Aspect qualified as NE
import Marvel.Card.Def qualified as NE
import Marvel.Ability.Types qualified as NEAbility

import Marvel.Prelude
import Marvel.Matchers
import Marvel.Resource
import Marvel.EncounterSet
import Marvel.Boost

import Marvel.Cost.Types (Cost (NoCost))

import Data.HashSet (singleton)
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Keyword
import Marvel.Name
import Marvel.Trait

allHeroCards :: [CardDef]
allHeroCards =
  [ dominoHero, cable40001a
  , gambitHero
  , stormHero
  , wolverineHero
  , cyclopsHero
  , spiderMan
  , captainMarvel
  , sheHulk
  , ironMan
  , blackPanther
  , captainAmerica
  , msMarvel
  , blackWidow
  , thor
  , doctorStrange
  , hulk
  -- The Green Goblin

  -- The Wrecking Crew

  -- The Rise of Red Skull
  , hawkeye
  , spiderWoman

  -- The Once and Future Kang

  -- Ant-Man
  , antManTiny
  , antManGiant

  -- Wasp
  , waspTiny
  , waspGiant

  -- Quicksilver
  , quicksilver

  -- Scarlet Witch
  , scarletWitch

  -- Star-Lord
  , starLord

  -- Gamora
  , gamora

  -- Groot
  , groot

  -- Rocket Raccoon
  , rocketRaccoon
  -- Drax
  , drax

  -- Venom
  , venom

  -- Spectrum
  , spectrum

  -- Adam Warlock
  , adamWarlockHero
  , nebulaHero
  , warMachineHero
  , valkyrieHero
  , visionHero
  , ghostSpider
  , spiderManMilesMoralesHero
  , novaHero
  , ironheartVersion1
  , ironheartVersion2
  , ironheartVersion3
  , spiderHam
  , spdrSuitHero
  , colossusHero
  , shadowcatHero
  , phoenixHero
  ]

allHeroesMap :: HashMap CardCode CardDef
allHeroesMap = fromList $ map (toFst toCardCode) allHeroCards

hero :: CardCode -> Name -> [Trait] -> CardDef
hero cardCode name traits = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = HeroType
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

spiderMan :: CardDef
spiderMan = hero "01001a" "Spider-Man" [Avenger]

captainMarvel :: CardDef
captainMarvel = hero "01010a" "Captain Marvel" [Avenger, Soldier]

sheHulk :: CardDef
sheHulk = hero "01019a" "She-Hulk" [Avenger, Gamma]

ironMan :: CardDef
ironMan = hero "01029a" "Iron Man" [Avenger]

blackPanther :: CardDef
blackPanther = (hero "01040a" "Black Panther" [Avenger, Wakanda])
  { cdKeywords = singleton (Retaliate 1)
  }

captainAmerica :: CardDef
captainAmerica = hero "03001a" "Captain America" [Avenger, Soldier]

msMarvel :: CardDef
msMarvel = hero "05001a" "Ms. Marvel" [Champion, Inhuman]
hulk :: CardDef
hulk = hero "10001a" "Hulk" [Avenger, Gamma]

blackWidow :: CardDef
blackWidow = hero "08001a" "Black Widow" [Avenger, Spy]

thor :: CardDef
thor = hero "06001a" "Thor" [Asgard, Avenger]

doctorStrange :: CardDef
doctorStrange = hero "09001a" "Doctor Strange" [Avenger, Mystic]

hawkeye :: CardDef
hawkeye = hero "04001a" "Hawkeye" [Avenger]

spiderWoman :: CardDef
spiderWoman = hero "04031a" "Spider-Woman" [Avenger, Spy]

antManTiny :: CardDef
antManTiny = hero "12001a" "Ant-Man" [Avenger, Tiny]

antManGiant :: CardDef
antManGiant = hero "12001c" "Ant-Man" [Avenger, Giant]

-- Wasp

waspTiny :: CardDef
waspTiny = hero "13001a" "Wasp" [Avenger, Tiny]

waspGiant :: CardDef
waspGiant = hero "13001c" "Wasp" [Avenger, Giant]

-- Quicksilver

quicksilver :: CardDef
quicksilver = hero "14001a" "Quicksilver" [Avenger]

-- Scarlet Witch

scarletWitch :: CardDef
scarletWitch = hero "15001a" "Scarlet Witch" [Avenger, Mystic]

-- Star-Lord

starLord :: CardDef
starLord = hero "17001a" "Star-Lord" [Guardian]

-- Gamora

gamora :: CardDef
gamora = hero "18001a" "Gamora" [Guardian]

-- Groot

groot :: CardDef
groot = hero "16001a" "Groot" [Guardian]

-- Rocket Raccoon

rocketRaccoon :: CardDef
rocketRaccoon = hero "16029a" "Rocket Raccoon" [Guardian]
-- Drax

drax :: CardDef
drax = hero "19001a" "Drax" [Guardian]

-- Venom

venom :: CardDef
venom = hero "20001a" "Venom" [Guardian, SpaceKnight]

-- Spectrum

-- | Unique, as printed (MarvelCDB has her not unique).
spectrum :: CardDef
spectrum = hero "21001a" "Spectrum" [Aerial, Avenger]

-- Adam Warlock

-- | Not the ally of the same name (17011).
adamWarlockHero :: CardDef
adamWarlockHero = hero "21031a" "Adam Warlock" [Guardian, Mystic]

-- Nebula

nebulaHero :: CardDef
nebulaHero = deckLimited 1 $ (hero "22001a" "Nebula" [Guardian])
    { cdUnique = True
    }

-- War Machine

warMachineHero :: CardDef
warMachineHero = deckLimited 1 $ (hero "23001a" "War Machine" [Avenger, Soldier])
    { cdUnique = True
    }

-- Valkyrie

valkyrieHero :: CardDef
valkyrieHero = deckLimited 1 $ (hero "25001a" "Valkyrie" [Asgard, Avenger])
    { cdUnique = True
    }

-- Vision

visionHero :: CardDef
visionHero = deckLimited 1 $ (hero "26001a" "Vision" [Android, Avenger])
    { cdUnique = True
    }


-- Sinister Motives

ghostSpider :: CardDef
ghostSpider = hero "27001a" "Ghost-Spider" [WebWarrior]

spiderManMilesMoralesHero :: CardDef
spiderManMilesMoralesHero = hero "27030a" "Spider-Man" [Champion, WebWarrior]

-- Nova

novaHero :: CardDef
novaHero = (hero "28001a" "Nova" [Champion])
  { cdDeckLimit = Just 1
  , cdUnique = True
  }


-- Ironheart

ironheartVersion1 :: CardDef
ironheartVersion1 = (hero "29001a" "Ironheart" [Champion, Version1])
  { cdUnique = True
  }

ironheartVersion2 :: CardDef
ironheartVersion2 = (hero "29002a" "Ironheart" [Aerial, Champion, Version2])
  { cdUnique = True
  }

ironheartVersion3 :: CardDef
ironheartVersion3 = (hero "29003a" "Ironheart" [Aerial, Champion, Version3])
  { cdUnique = True
  }


-- Spider-Ham

spiderHam :: CardDef
spiderHam = hero "30001a" "Spider-Ham" [Cartoon, WebWarrior]


-- SP//dr

spdrSuitHero :: CardDef
spdrSuitHero = (hero "31001a" "SP//dr Suit" [Active, WebWarrior])
  { cdUnique = True
  }

-- Mutant Genesis

colossusHero :: CardDef
colossusHero = (hero "32001a" "Colossus" [XMen])
  { cdUnique = True
  }

-- Mutant Genesis

shadowcatHero :: CardDef
shadowcatHero = (hero "32030a" "Shadowcat" [XMen])
  { cdUnique = True
  }


-- Phoenix

phoenixHero :: CardDef
phoenixHero = hero "34001a" "Phoenix" [Psionic, XMen]


-- Cyclops

cyclopsHero :: CardDef
cyclopsHero = (hero "33001a" "Cyclops" [XMen]) { cdDeckLimit = Just 1, cdUnique = True }


-- Storm

stormHero :: CardDef
stormHero = (hero "36001a" "Storm" [XMen]) { cdUnique = True }

-- Wolverine

wolverineHero :: CardDef
wolverineHero = (hero "35001a" "Wolverine" [XMen]) {cdUnique = True}

-- Gambit

gambitHero :: CardDef
gambitHero = (hero "37001a" "Gambit" [Thief, XMen]) { cdUnique = True }


-- NeXt Evolution: Cable

cable40001a :: CardDef
cable40001a = (hero "40001a" "Cable" [NE.Soldier, NE.XForce]) { cdUnique = True, cdDeckLimit = Just 1 }

-- NeXt Evolution: Domino
dominoHero :: CardDef
dominoHero = CardDef
  { cdCardCode = "40037a", cdName = "Domino", cdCost = Nothing
  , cdTraits = fromList [NET.Posse, NET.XForce], cdKeywords = fromList []
  , cdCardType = HeroType, cdAbilityType = Nothing, cdAbilitySubTypes = []
  , cdUnique = True, cdAspect = Nothing, cdCriteria = NoCriteria
  , cdResources = [], cdBoostIcons = []
  , cdEncounterSet = Nothing, cdEncounterSetQuantity = Nothing
  , cdResponseWindow = Nothing, cdActionCriteria = Nothing, cdHazards = 0, cdAcceleration = 0, cdAmplify = 0
  , cdLimit = Nothing, cdDeckLimit = Nothing, cdTeamUp = Nothing
  , cdAlternateCardCodes = [], cdPlayableFromDiscard = False, cdDoubleWildResources = False
  , cdDoesNotCountTowardHandSize = False, cdCanPayForOthersWithTrait = Nothing, cdAbilityCost = NoCost
  }
