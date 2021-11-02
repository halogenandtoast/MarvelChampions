module Marvel.Card.Def where

import Marvel.Prelude

import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Name
import Marvel.Resource
import Marvel.Trait

data CardType
  = AlterEgoType
  | HeroType
  | VillainType
  | AllyType
  | EventType
  | SupportType
  | ResourceType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdCost :: Maybe Int
  , cdTraits :: HashSet Trait
  , cdCardType :: CardType
  , cdUnique :: Bool
  , cdAspect :: Maybe Aspect
  , cdResources :: [Resource]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardCode CardDef where
  toCardCode = cdCardCode
