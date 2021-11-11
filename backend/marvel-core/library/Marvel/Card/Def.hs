{-# LANGUAGE UndecidableInstances #-}
module Marvel.Card.Def where

import Marvel.Prelude

import GHC.Generics
import Marvel.Aspect
import Marvel.Boost
import Marvel.Card.Code
import {-# SOURCE #-} Marvel.Card.PlayerCard
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Keyword
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
  | AttachmentType
  | MinionType
  | TreacheryType
  | SideSchemeType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardMatcher = AnyCard | CardWithAspect Aspect | CardWithResource Resource
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

cardMatch :: HasCardDef a => CardMatcher -> a -> Bool
cardMatch matcher a = case matcher of
  AnyCard -> True
  CardWithAspect aspect -> cdAspect def == Just aspect
  CardWithResource resource ->
    resource `elem` map snd (filter isPrintedResource $ cdResources def)
 where
  def = getCardDef a
  isPrintedResource (restriction, _) = restriction == PrintedResource

data ResourceRestriction = PrintedResource | ResourceForCardsMatching CardMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdCost :: Maybe Int
  , cdTraits :: HashSet Trait
  , cdKeywords :: HashSet Keyword
  , cdCardType :: CardType
  , cdUnique :: Bool
  , cdAspect :: Maybe Aspect
  , cdCriteria :: Criteria
  , cdResources :: [(ResourceRestriction, Resource)]
  , cdBoostIcons :: [BoostIcon]
  , cdEncounterSet :: Maybe EncounterSet
  , cdEncounterSetQuantity :: Maybe Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

resourcesL :: Lens' CardDef [(ResourceRestriction, Resource)]
resourcesL = lens cdResources $ \m x -> m { cdResources = x }

instance HasCardCode CardDef where
  toCardCode = cdCardCode

class HasCardDef a where
  getCardDef :: a -> CardDef

class HasCardDef' f where
  getCardDef' :: f p -> CardDef

genericGetCardDef :: (Generic a, HasCardDef' (Rep a)) => a -> CardDef
genericGetCardDef = getCardDef' . from
