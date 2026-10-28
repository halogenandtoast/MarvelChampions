module Marvel.Hero.Cards where

import Marvel.Prelude

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
  [ spiderMan
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
  , cdAlternateCardCodes = []
  , cdPlayableFromDiscard = False
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
