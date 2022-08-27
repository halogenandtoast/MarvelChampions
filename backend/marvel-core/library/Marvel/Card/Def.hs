module Marvel.Card.Def where

import Marvel.Prelude

import Marvel.Ability.Type
import Marvel.Aspect
import Marvel.Boost
import Marvel.Card.Code
import Marvel.Criteria
import Marvel.EncounterSet
import Marvel.Keyword
import Marvel.Name
import Marvel.Resource.Types
import Marvel.Trait.Types
import Marvel.Window.Types

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
  | MainSchemeType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data CardMatcher
  = AnyCard
  | CardWithAspect Aspect
  | CardWithResource Resource
  | CardWithType CardType
  | CardWithTrait Trait
  | CardMatchesAll [CardMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Semigroup CardMatcher where
  AnyCard <> x = x
  x <> AnyCard = x
  CardMatchesAll xs <> CardMatchesAll ys = CardMatchesAll $ xs <> ys
  x <> CardMatchesAll ys = CardMatchesAll $ x : ys
  CardMatchesAll xs <> y = CardMatchesAll $ xs <> [y]
  x <> y = CardMatchesAll [x, y]

instance Monoid CardMatcher where
  mempty = AnyCard

cardMatch :: HasCardDef a => CardMatcher -> a -> Bool
cardMatch matcher a = case matcher of
  AnyCard -> True
  CardWithAspect aspect -> cdAspect def == Just aspect
  CardWithType cType -> cdCardType def == cType
  CardWithTrait trait -> trait `member` cdTraits def
  CardWithResource resource ->
    resource `elem` map snd (filter isPrintedResource $ cdResources def)
  CardMatchesAll xs -> all (`cardMatch` a) xs
 where
  def = getCardDef a
  isPrintedResource (restriction, _) = restriction == PrintedResource

data ResourceRestriction
  = PrintedResource
  | ResourceForCardsMatching CardMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data InPlayLimit = MaxPerPlayer Natural | MaxPerEnemy Natural | MaxPerAlly Natural
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
  , cdAbilitySubType :: Maybe AbilitySubType
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
  , cdLimit :: Maybe InPlayLimit
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

toAdditionalCriteria :: CardDef -> Criteria
toAdditionalCriteria def = case cdAbilityType def of
  Just aType -> case aType of
    HeroInterrupt -> InHeroForm
    HeroResource -> InHeroForm
    HeroAction -> InHeroForm
    AlterEgoAction -> InAlterEgoForm
    _ -> NoCriteria
  Nothing -> NoCriteria
