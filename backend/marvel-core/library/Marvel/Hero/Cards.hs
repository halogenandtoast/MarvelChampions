module Marvel.Hero.Cards where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Criteria
import Marvel.Name
import Marvel.Trait

allHeroCards :: [CardDef]
allHeroCards = [spiderMan, captainMarvel]

allHeroesMap :: HashMap CardCode CardDef
allHeroesMap = fromList $ map (toCardCode &&& id) allHeroCards

hero :: CardCode -> Name -> [Trait] -> CardDef
hero cardCode name traits = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdCost = Nothing
  , cdTraits = fromList traits
  , cdCardType = HeroType
  , cdUnique = True
  , cdAspect = Nothing
  , cdCriteria = NoCriteria
  , cdResources = []
  }

spiderMan :: CardDef
spiderMan = hero "01001a" "Peter Parker" [Avenger]

captainMarvel :: CardDef
captainMarvel = hero "01010a" "Captain Marvel" [Avenger, Soldier]
