{-# LANGUAGE UndecidableInstances #-}
module Marvel.Card.Def where

import Marvel.Prelude

import GHC.Generics
import Marvel.Aspect
import Marvel.Card.Code
import {-# SOURCE #-} Marvel.Card.PlayerCard
import Marvel.Name
import Marvel.Resource
import Marvel.Trait

class HasResources a where
  resourcesFor :: a -> PlayerCard -> [Resource]

data CardType
  = AlterEgoType
  | HeroType
  | VillainType
  | AllyType
  | EventType
  | SupportType
  | ResourceType
  | UpgradeType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardMatcher = AnyCard | CardWithAspect Aspect
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

cardMatch :: HasCardDef a => CardMatcher -> a -> Bool
cardMatch matcher a = case matcher of
  AnyCard -> True
  CardWithAspect aspect -> cdAspect (getCardDef a) == Just aspect

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdCost :: Maybe Int
  , cdTraits :: HashSet Trait
  , cdCardType :: CardType
  , cdUnique :: Bool
  , cdAspect :: Maybe Aspect
  , cdResources :: [(CardMatcher, Resource)]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

resourcesL :: Lens' CardDef [(CardMatcher, Resource)]
resourcesL = lens cdResources $ \m x -> m { cdResources = x }

instance HasCardCode CardDef where
  toCardCode = cdCardCode

class HasCardDef a where
  getCardDef :: a -> CardDef

class HasCardDef' f where
  getCardDef' :: f p -> CardDef

genericGetCardDef :: (Generic a, HasCardDef' (Rep a)) => a -> CardDef
genericGetCardDef = getCardDef' . from
