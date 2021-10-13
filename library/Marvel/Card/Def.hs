module Marvel.Card.Def where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Name
import Marvel.Trait

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdTraits :: HashSet Trait
  }
  deriving stock (Show, Eq)

instance HasCardCode CardDef where
  toCardCode = cdCardCode

