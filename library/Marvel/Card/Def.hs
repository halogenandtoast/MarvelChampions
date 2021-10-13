module Marvel.Card.Def where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Name
import Marvel.Trait

data CardType = AlterEgoType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdTraits :: HashSet Trait
  , cdCardType :: CardType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardCode CardDef where
  toCardCode = cdCardCode
