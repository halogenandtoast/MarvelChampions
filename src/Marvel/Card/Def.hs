module Marvel.Card.Def where

import Marvel.Prelude

import Marvel.Ability.Types
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
  | EnvironmentType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data CardMatcher
  = AnyCard
  | CardWithAspect Aspect
  | CardWithResource Resource
  | CardWithType CardType
  | CardWithTrait Trait
  | CardMatchesAll [CardMatcher]
  | -- | the card, in any printing
    CardIs CardDef
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
  CardIs def' -> sameCard def' def
 where
  def = getCardDef a
  isPrintedResource (restriction, _) = restriction == PrintedResource

data ResourceRestriction
  = PrintedResource
  | ResourceForCardsMatching CardMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data InPlayLimit
  = MaxPerPlayer Natural
  | MaxPerEnemy Natural
  | MaxPerAlly Natural
  | -- | "Play under any player's control. Max N per player."
    MaxPerPlayerUnderAnyControl Natural
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
  , -- | the codes the card is also printed under: a reprint, an alternate art
    cdAlternateCardCodes :: [CardCode]
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

-- | The card under each of its printings, its own code first: every printing is the
-- same card with that printing's code, as ArkhamHorror's 'toCardCodePairs' has it.
toCardCodePairs :: CardDef -> [(CardCode, CardDef)]
toCardCodePairs c =
  (cdCardCode c, c)
    : [ (code, c {cdCardCode = code, cdAlternateCardCodes = map swap' (cdAlternateCardCodes c)})
      | code <- cdAlternateCardCodes c
      , let swap' c' = if c' == code then cdCardCode c else c'
      ]

-- | The card is also printed under these codes.
reprintedAs :: [CardCode] -> CardDef -> CardDef
reprintedAs codes def = def {cdAlternateCardCodes = codes}

-- | A printing of the card under its own code, for a reprint whose def differs, as one in
-- another encounter set; register its entity with 'reprintAs'.
reprint :: CardCode -> CardDef -> CardDef
reprint code def = def {cdCardCode = code, cdAlternateCardCodes = [cdCardCode def]}

-- | A key every printing of a card shares: the lowest of its codes.
canonicalCardCode :: CardDef -> CardCode
canonicalCardCode c = foldl' min (cdCardCode c) (cdAlternateCardCodes c)

-- | Are these two defs the same card, perhaps in different printings?
sameCard :: CardDef -> CardDef -> Bool
sameCard a b = canonicalCardCode a == canonicalCardCode b

class HasCardDef a where
  getCardDef :: a -> CardDef

toAdditionalCriteria :: CardDef -> Criteria
toAdditionalCriteria = maybe NoCriteria abilityTypeCriteria . cdAbilityType
