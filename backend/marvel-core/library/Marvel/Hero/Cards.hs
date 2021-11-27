module Marvel.Hero.Cards where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Trait

allHeroCards :: [CardDef]
allHeroCards = [spiderMan, captainMarvel, sheHulk]

allHeroesMap :: HashMap CardCode CardDef
allHeroesMap = fromList $ map (toCardCode &&& id) allHeroCards

hero :: CardCode -> Name -> [Trait] -> CardDef
hero cardCode name traits = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdKeywords = mempty
  , cdCardType = HeroType
  , cdAbilityType = Nothing
  , cdAbilitySubType = Nothing
  , cdUnique = True
  , cdAspect = Nothing
  , cdEncounterSet = Nothing
  , cdEncounterSetQuantity = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  , cdResponseWindow = Nothing
  , cdBoostIcons = []
  , cdHazards = 0
  , cdAcceleration = 0
  }

spiderMan :: CardDef
spiderMan = hero "01001a" "Spider-Man" [Avenger]

captainMarvel :: CardDef
captainMarvel = hero "01010a" "Captain Marvel" [Avenger, Soldier]

sheHulk :: CardDef
sheHulk = hero "01019a" "She-Hulk" [Avenger, Gamma]
