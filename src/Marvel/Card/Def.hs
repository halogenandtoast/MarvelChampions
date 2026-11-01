module Marvel.Card.Def where

import Marvel.Prelude
import Data.Aeson.KeyMap qualified as KeyMap

import Marvel.Ability.Types
import Marvel.Aspect
import Marvel.Boost
import Marvel.Card.Code
import Marvel.Cost.Types
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
  | PlayerSideSchemeType
  | MainSchemeType
  | EnvironmentType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data CardMatcher
  = AnyCard
  | CardWithAspect Aspect
  | CardWithResource Resource
  | CardWithAbilityType AbilityType
  | CardWithType CardType
  | CardWithTrait Trait
  | CardMatchesAll [CardMatcher]
  | CardWithPrintedCostAtMost Natural
  | CardWithPrintedCostAtLeast Natural
  | -- | the card in any of its printings
    CardIs CardDef
  | CardMatchesAny [CardMatcher]
  | -- | a player card of the hero's own, of no aspect and not basic
    HeroSpecificCard
  | CardInEncounterSet EncounterSet
  | CardWithTitle Text
  | NotCardMatching CardMatcher
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
  CardWithAbilityType ability -> cdAbilityType def == Just ability
  CardWithType cType -> cdCardType def == cType
  CardWithTrait trait -> trait `member` cdTraits def
  CardWithResource resource ->
    resource `elem` map snd (filter isPrintedResource $ cdResources def)
  CardMatchesAll xs -> all (`cardMatch` a) xs
  CardWithPrintedCostAtMost n -> maybe False (\cost -> toInteger (max 0 cost) <= toInteger n) (cdCost def)
  CardWithPrintedCostAtLeast n -> maybe False (\cost -> toInteger (max 0 cost) >= toInteger n) (cdCost def)
  CardIs def' -> sameCard def' def
  CardMatchesAny xs -> any (`cardMatch` a) xs
  HeroSpecificCard -> isNothing (cdAspect def) && cdCardType def `elem` [AllyType, EventType, ResourceType, SupportType, UpgradeType, PlayerSideSchemeType]
  CardInEncounterSet set -> cdEncounterSet def == Just set
  CardWithTitle t -> title (cdName def) == t
  NotCardMatching m -> not (cardMatch m a)
 where
  def = getCardDef a
  isPrintedResource (restriction, _) = restriction == PrintedResource

data ResourceRestriction
  = PrintedResource
  | ResourceForCardsMatching CardMatcher
  | -- | one for each ally its owner controls, to this maximum (Band Together): not printed
    ForEachAllyControlled Natural
  | -- | extra resources for physical acceleration tokens on the owner's main scheme,
    -- capped at this many (Montage); these are not printed icons.
    ForEachMainSchemeAccelerationToken Natural
  | -- | one additional resource if the owner's sustained damage is below this value
    -- (Self Confidence, Self Control and Self Preservation); not a printed icon.
    ResourceIfIdentityDamageLessThan Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data InPlayLimit
  = MaxPerPlayer Natural
  | MaxPerEnemy Natural
  | MaxPerAlly Natural
  | -- | "Play under any player's control. Max N per player."
    MaxPerPlayerUnderAnyControl Natural
  | MaxPerScheme Natural
  | -- | "Max N per round": copies played this round, by any player
    MaxPerRound Natural
  | -- | "Max N per phase": copies played this phase, by any player
    MaxPerPhase Natural
  | -- | copies of this title played during one triggering occurrence
    MaxPerWindow Natural
  | MaxPerCharacter Natural
  | MaxPerSideScheme Natural
  | -- | "Play under any player's control.", with no maximum
    UnderAnyControl
  | -- | "Play under any player's control. Max N <Trait> per player." (Mighty Avengers):
    -- cards with the trait, supports and upgrades, a player controls
    MaxTraitPerPlayerUnderAnyControl Trait Natural
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
  , -- | its labels: "(attack/thwart)" is both (RR Labeled Ability)
    cdAbilitySubTypes :: [AbilitySubType]
  , cdUnique :: Bool
  , cdAspect :: Maybe Aspect
  , cdCriteria :: Criteria
  , cdResources :: [(ResourceRestriction, Resource)]
  , cdBoostIcons :: [BoostIcon]
  , cdEncounterSet :: Maybe EncounterSet
  , cdEncounterSetQuantity :: Maybe Natural
  , cdResponseWindow :: Maybe WindowMatcher
  , -- | a card played in its window that is also played as an action, under these
    -- criteria (First Hit)
    cdActionCriteria :: Maybe Criteria
  , cdHazards :: Natural
  , cdAcceleration :: Natural
  , -- | its amplify icons: each boost card turned faceup during an enemy activation
    -- gains a boost icon for each amplify icon in play (RR Amplify Icon)
    cdAmplify :: Natural
  , cdLimit :: Maybe InPlayLimit
  , -- | its deck-copy limit, shared across all printings; absent in legacy saves
    cdDeckLimit :: Maybe Natural
  , -- | the named identities permitted to include this Team-Up in their deck
    cdTeamUp :: Maybe (Text, Text)
  , -- | the codes the card is also printed under: a reprint, an alternate art
    cdAlternateCardCodes :: [CardCode]
  , -- | may be played from its owner's discard pile, during their turn
    cdPlayableFromDiscard :: Bool
  , -- | an event's cost before the arrow, paid with its resource cost
    cdAbilityCost :: Cost
  , cdDoubleWildResources :: Bool
  , cdDoesNotCountTowardHandSize :: Bool
  , cdCanPayForOthersWithTrait :: Maybe Trait
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, Hashable)

instance FromJSON CardDef where
  parseJSON = withObject "CardDef" $ \o -> genericParseJSON defaultOptions $ Object $
    foldr (\(k, v) -> KeyMap.insertWith (\_ existing -> existing) k v) o
      [("cdDoubleWildResources", Bool False), ("cdDoesNotCountTowardHandSize", Bool False), ("cdCanPayForOthersWithTrait", Null)]

-- | Generation doubles before wild resources are declared as their payment types.
doubleWildResources :: Maybe CardDef -> [Resource] -> [Resource]
doubleWildResources def = concatMap (\r -> replicate (if r == Wild && maybe False cdDoubleWildResources def then 2 else 1) r)

deckLimited :: Natural -> CardDef -> CardDef
deckLimited n def = def {cdDeckLimit = Just n}

-- | Resolve the printed per-player value, before play-cost increases/reductions.
-- Keep the printed coefficient rather than multiplying a previously resolved cost:
-- physical cards can move, be rebuilt as entities, or be reloaded repeatedly.
resolvePlayerCountCost :: Int -> CardDef -> CardDef
resolvePlayerCountCost players def = case
  [coefficient | PerPlayerResourceCost coefficient <- toList (cdKeywords def)] of
  coefficient : _ -> def {cdCost = Just $ fromIntegral coefficient * max 0 players}
  [] -> def

teamUpNames :: Text -> Text -> CardDef -> CardDef
teamUpNames a b def = def {cdTeamUp = Just (a, b)}

resourcesL :: Lens' CardDef [(ResourceRestriction, Resource)]
resourcesL = lens cdResources $ \m x -> m { cdResources = x }

printedResources :: CardDef -> [Resource]
printedResources = mapMaybe printedResource . cdResources
 where
  printedResource (PrintedResource, a) = Just a
  printedResource _ = Nothing

-- | Requirement icons constrain the resources spent; they are not an additional
-- resource cost. Reductions cannot remove these mandatory icons.
requiredResources :: HasCardDef a => a -> [Resource]
requiredResources a = concat [rs | Requirement rs <- toList (cdKeywords $ getCardDef a)]

requirementResourceCost :: [Resource] -> Int -> [Maybe Resource]
requirementResourceCost required cost =
  map Just required <> replicate (max 0 (cost - length required)) Nothing

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

instance HasCardDef CardDef where
  getCardDef = id

toAdditionalCriteria :: CardDef -> Criteria
toAdditionalCriteria def =
  maybe NoCriteria abilityTypeCriteria (cdAbilityType def)
    <> foldMap abilitySubTypeCriteria (cdAbilitySubTypes def)
