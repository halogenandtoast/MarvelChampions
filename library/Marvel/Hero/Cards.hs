module Marvel.Hero.Cards where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Name
import Marvel.Trait

allHeroCards :: [CardDef]
allHeroCards = [spiderMan, captainMarvel]

allHerosMap :: HashMap CardCode CardDef
allHerosMap = fromList $ map (toCardCode &&& id) allHeroCards

hero :: CardCode -> Name -> [Trait] -> CardDef
hero cardCode name traits = CardDef
  { cdCardCode = cardCode
  , cdName = name
  , cdTraits = fromList traits
  , cdCardType = HeroType
  }

spiderMan :: CardDef
spiderMan = hero "01001a" "Peter Parker" [Avenger]

captainMarvel :: CardDef
captainMarvel = hero "01010a" "Captain Marvel" [Avenger, Soldier]
