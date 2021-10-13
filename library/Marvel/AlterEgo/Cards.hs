module Marvel.AlterEgo.Cards where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Name
import Marvel.Trait

alterEgo :: CardCode -> Name -> [Trait] -> CardDef
alterEgo cardCode name traits =
  CardDef { cdCardCode = cardCode, cdName = name, cdTraits = fromList traits }

peterParker :: CardDef
peterParker = alterEgo "01001a" "Peter Parker" [Genius]

carolDanvers :: CardDef
carolDanvers = alterEgo "01010a" "Carol Danvers" [Shield, Soldier]
