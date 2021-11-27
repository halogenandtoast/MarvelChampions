module Marvel.Card.Def where

import Marvel.Prelude

import GHC.Generics
import Marvel.Ability.Type
import Marvel.Aspect
import Marvel.Boost
import Marvel.Card.Code
import {-# SOURCE #-} Marvel.Card.PlayerCard
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Name
import Marvel.Resource
import Marvel.Trait
import Marvel.Window

class HasResources a where
  resourcesFor :: MonadGame env m => a -> Maybe PlayerCard -> m [Resource]

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
  | ObligationType
  | TreacheryType
  | SideSchemeType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data CardMatcher
  = AnyCard
  | CardWithAspect Aspect
  | CardWithResource Resource
  | CardWithType CardType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

cardMatch :: HasCardDef a => CardMatcher -> a -> Bool
cardMatch matcher a = case matcher of
  AnyCard -> True
  CardWithAspect aspect -> cdAspect def == Just aspect
  CardWithType cType -> cdCardType def == cType
  CardWithResource resource ->
    resource `elem` map snd (filter isPrintedResource $ cdResources def)
 where
  def = getCardDef a
  isPrintedResource (restriction, _) = restriction == PrintedResource

data ResourceRestriction = PrintedResource | ResourceForCardsMatching CardMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdCost :: Maybe Int
  , cdTraits :: HashSet Trait
  , cdKeywords :: HashSet Keyword
  , cdCardType :: CardType
  , cdAbilityType :: Maybe AbilityType
  , cdUnique :: Bool
  , cdAspect :: Maybe Aspect
  , cdCriteria :: Criteria
  , cdResources :: [(ResourceRestriction, Resource)]
  , cdBoostIcons :: [BoostIcon]
  , cdEncounterSet :: Maybe EncounterSet
  , cdEncounterSetQuantity :: Maybe Natural
  , cdResponseWindow :: Maybe WindowMatcher
  , cdHazards :: Natural
  , cdAcceleration :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

resourcesL :: Lens' CardDef [(ResourceRestriction, Resource)]
resourcesL = lens cdResources $ \m x -> m { cdResources = x }

printedResources :: CardDef -> [Resource]
printedResources = mapMaybe printedResource . cdResources
 where
  printedResource (PrintedResource, a) = Just a
  printedResource _ = Nothing

instance HasCardCode CardDef where
  toCardCode = cdCardCode

class HasCardDef a where
  getCardDef :: a -> CardDef

class HasCardDef' f where
  getCardDef' :: f p -> CardDef

genericGetCardDef :: (Generic a, HasCardDef' (Rep a)) => a -> CardDef
genericGetCardDef = getCardDef' . from
