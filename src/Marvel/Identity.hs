{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Identity (
  module Marvel.Identity,
  module Marvel.Identity.Types,
) where

import Marvel.Prelude
import Control.Monad (replicateM_)

import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (find, partition, sortOn)
import Marvel.Ability
import Marvel.ActiveCost
import Marvel.AlterEgo
import Marvel.AlterEgo.Types
import Marvel.Attack
import Marvel.Card
import Marvel.Campaign qualified as Campaign
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Deck
import Marvel.Discard
import Marvel.EncounterCard
import Marvel.EncounterSet
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero
import Marvel.Hero.Runner
import Marvel.Identity.Types
import Marvel.Keyword hiding (Setup)
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Name (Name (..))
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Queue (HasQueue)
import Marvel.Ref
import Marvel.Resource
import Marvel.Resource.Resources.MolecularAcceleration (spendingInterruptCounters)
import Marvel.Trait
import Marvel.Window qualified as W
import System.Random.Shuffle

passedL :: Lens' (Attrs PlayerIdentity) Bool
passedL = lens playerIdentityPassed $ \m x -> m {playerIdentityPassed = x}

exhaustedL :: Lens' (Attrs PlayerIdentity) Bool
exhaustedL =
  lens playerIdentityExhausted $ \m x -> m {playerIdentityExhausted = x}

encounterCardsL :: Lens' (Attrs PlayerIdentity) [EncounterCard]
encounterCardsL = lens playerIdentityEncounterCards $
  \m x -> m {playerIdentityEncounterCards = x}

damageL :: Lens' (Attrs PlayerIdentity) Natural
damageL = lens playerIdentityDamage $ \m x -> m {playerIdentityDamage = x}

sideL :: Lens' (Attrs PlayerIdentity) Side
sideL = lens playerIdentitySide $ \m x -> m {playerIdentitySide = x}

sidesL :: Lens' (Attrs PlayerIdentity) (HashMap Side PlayerIdentitySide)
sidesL = lens playerIdentitySides $ \m x -> m {playerIdentitySides = x}

alliesL :: Lens' (Attrs PlayerIdentity) (HashSet AllyId)
alliesL = lens playerIdentityAllies $ \m x -> m {playerIdentityAllies = x}

minionsL :: Lens' (Attrs PlayerIdentity) (HashSet MinionId)
minionsL = lens playerIdentityMinions $ \m x -> m {playerIdentityMinions = x}

upgradesL :: Lens' (Attrs PlayerIdentity) (HashSet UpgradeId)
upgradesL =
  lens playerIdentityUpgrades $ \m x -> m {playerIdentityUpgrades = x}

supportsL :: Lens' (Attrs PlayerIdentity) (HashSet SupportId)
supportsL =
  lens playerIdentitySupports $ \m x -> m {playerIdentitySupports = x}

deckL :: Lens' (Attrs PlayerIdentity) Deck
deckL = lens playerIdentityDeck $ \m x -> m {playerIdentityDeck = x}

handL :: Lens' (Attrs PlayerIdentity) Hand
handL = lens playerIdentityHand $ \m x -> m {playerIdentityHand = x}

discardL :: Lens' (Attrs PlayerIdentity) Discard
discardL = lens playerIdentityDiscard $ \m x -> m {playerIdentityDiscard = x}

stunnedL :: Lens' (Attrs PlayerIdentity) Bool
stunnedL = lens playerIdentityStunned $ \m x -> m {playerIdentityStunned = x}

confusedL :: Lens' (Attrs PlayerIdentity) Bool
confusedL =
  lens playerIdentityConfused $ \m x -> m {playerIdentityConfused = x}

toughL :: Lens' (Attrs PlayerIdentity) Bool
toughL = lens playerIdentityTough $ \m x -> m {playerIdentityTough = x}

defeatedL :: Lens' (Attrs PlayerIdentity) Bool
defeatedL =
  lens playerIdentityDefeated $ \m x -> m {playerIdentityDefeated = x}

defendedL :: Lens' (Attrs PlayerIdentity) Bool
defendedL =
  lens playerIdentityDefended $ \m x -> m {playerIdentityDefended = x}

damageReductionL :: Lens' (Attrs PlayerIdentity) Natural
damageReductionL = lens playerIdentityDamageReduction $
  \m x -> m {playerIdentityDamageReduction = x}

statusL :: StatusCard -> Lens' (Attrs PlayerIdentity) Bool
statusL = \case
  StunnedStatus -> stunnedL
  ConfusedStatus -> confusedL
  ToughStatus -> toughL

identityIsStunned :: (HasGame m) => PlayerIdentity -> m Bool
identityIsStunned p = do
  steady <- elem Steady <$> getModifiedKeywords p
  let a = toAttrs p
  pure $ playerIdentityStunned a && (not steady || playerIdentityAdditionalStun a)

identityIsConfused :: (HasGame m) => PlayerIdentity -> m Bool
identityIsConfused p = do
  steady <- elem Steady <$> getModifiedKeywords p
  let a = toAttrs p
  pure $ playerIdentityConfused a && (not steady || playerIdentityAdditionalConfuse a)

-- | Adding a status card, as distinct from the character being stunned or confused.
identityGivenStatus :: (HasGame m) => PlayerIdentity -> StatusCard -> m PlayerIdentity
identityGivenStatus p s = do
  keywords <- getModifiedKeywords p
  modifiers <- getModifiers p
  let a = toAttrs p
  pure $ PlayerIdentity $ if Stalwart `elem` keywords && s /= ToughStatus then a else case s of
    ToughStatus | playerIdentityTough a -> a {playerIdentityAdditionalTough = if CanHaveAnyNumberOfToughStatusCards `elem` modifiers then playerIdentityAdditionalTough a + 1 else max (playerIdentityAdditionalTough a) $ min (sum [n | AdditionalToughStatusCards n <- modifiers]) (playerIdentityAdditionalTough a + 1)}
    StunnedStatus | Steady `elem` keywords && playerIdentityStunned a -> a {playerIdentityAdditionalStun = True}
    ConfusedStatus | Steady `elem` keywords && playerIdentityConfused a -> a {playerIdentityAdditionalConfuse = True}
    _ -> a & statusL s .~ True

removeOneIdentityTough :: Attrs PlayerIdentity -> Attrs PlayerIdentity
removeOneIdentityTough attrs
  | playerIdentityAdditionalTough attrs > 0 = attrs {playerIdentityAdditionalTough = playerIdentityAdditionalTough attrs - 1}
  | otherwise = attrs & toughL .~ False

isHero :: PlayerIdentity -> Bool
isHero player = case currentIdentity player of
  HeroSide _ -> True
  AlterEgoSide _ -> False

isAlterEgo :: PlayerIdentity -> Bool
isAlterEgo player = case currentIdentity player of
  HeroSide _ -> False
  AlterEgoSide _ -> True

identityDamage :: PlayerIdentity -> Natural
identityDamage = playerIdentityDamage . toAttrs

countedHandSize :: [PlayerCard] -> Int
countedHandSize = length . filter (not . cdDoesNotCountTowardHandSize . getCardDef)

getModifiedHandSize :: (HasGame m) => PlayerIdentity -> m Natural
getModifiedHandSize pid = do
  modifiers <- getModifiers pid
  let raised = foldr applyModifier (unHandSize $ handSize pid) modifiers
  pure $ subtractNatural (sum [n | HandSizeReduction n <- modifiers]) raised
 where
  applyModifier (HandSizeModifier n) = max 0 . (+ fromIntegral n)
  applyModifier (HandSizePenalty n) = subtractNatural n
  applyModifier _ = id

getModifiedHp :: (HasGame m) => PlayerIdentity -> m Natural
getModifiedHp pid@(PlayerIdentity attrs) = do
  modifiers <- getModifiers pid
  pure $ foldr applyModifier (unHp $ playerIdentityHP attrs) modifiers
 where
  applyModifier (HitPointModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

instance HasTraits PlayerIdentity where
  getTraits pIdentity = do
    traits <- case currentIdentity pIdentity of
      HeroSide x -> getTraits x
      AlterEgoSide x -> getTraits x
    (traits <>) <$> getGainedTraits pIdentity

-- | The hero's THW; none in alter-ego form.
getIdentityHeroThwart :: (HasGame m) => PlayerIdentity -> m Natural
getIdentityHeroThwart attrs = case currentIdentity attrs of
  AlterEgoSide _ -> pure 0
  HeroSide x -> getModifiedThwart $ toAttrs x

getIdentityHeroAttackDamage :: (HasGame m) => PlayerIdentity -> m Natural
getIdentityHeroAttackDamage attrs = case currentIdentity attrs of
  AlterEgoSide _ -> pure 0
  HeroSide x -> getModifiedAttack $ toAttrs x

instance Exhaustable PlayerIdentity where
  isExhausted = playerIdentityExhausted . toAttrs

instance HasResources PlayerIdentity where
  resourcesFor player card =
    concatMapM
      (`resourcesFor` card)
      (unHand $ playerIdentityHand $ toAttrs player)

instance IsRef PlayerIdentity where
  toRef = IdentityRef . toId

-- | Ironheart's current version, on either face of her identity.
ironheartVersion :: (HasGame m) => IdentityId -> m Natural
ironheartVersion ident = do
  code <- getIdentityCode ident
  pure $ case code of
    "29001a" -> 1
    "29001b" -> 1
    "29002a" -> 2
    "29002b" -> 2
    "29003a" -> 3
    "29003b" -> 3
    _ -> 0

currentIdentity :: PlayerIdentity -> PlayerIdentitySide
currentIdentity (PlayerIdentity a) =
  case lookup (playerIdentitySide a) (playerIdentitySides a) of
    Nothing -> error "Should not happen"
    Just s -> s

createIdentity ::
  IdentityId -> PlayerIdentitySide -> PlayerIdentitySide -> PlayerIdentity
createIdentity ident alterEgoSide heroSide =
  PlayerIdentity $
    PlayerIdentityAttrs
      { playerIdentitySide = B
      , playerIdentitySides = fromList [(A, heroSide), (B, alterEgoSide)]
      , playerIdentityId = ident
      , playerIdentityName = ""
      , playerIdentitySearching = False
      , playerIdentityHP = HP . fromIntegral $ gameValue (unHp hp) 0
      , playerIdentityDamage = 0
      , playerIdentityDeck = Deck []
      , playerIdentityDiscard = Discard []
      , playerIdentityHand = Hand []
      , playerIdentityPassed = False
      , playerIdentityAllies = mempty
      , playerIdentityAllyLimit = 3
      , playerIdentityMinions = mempty
      , playerIdentitySupports = mempty
      , playerIdentityUpgrades = mempty
      , playerIdentityExhausted = False
      , playerIdentityEncounterCards = []
      , playerIdentityBoostCards = []
      , playerIdentityDamageReduction = 0
      , playerIdentityStunned = False
      , playerIdentityConfused = False
      , playerIdentityAdditionalStun = False
      , playerIdentityAdditionalConfuse = False
      , playerIdentityTough = False
      , playerIdentityAdditionalTough = 0
      , playerIdentityDefeated = False
      , playerIdentityDefended = False
      , playerIdentityAttackDamageTaken = 0
      , playerIdentityWeatherDeck = Nothing
      , playerIdentityCounters = 0
      }
 where
  hp = case alterEgoSide of
    AlterEgoSide x -> startingHP x
    HeroSide x -> startingHP x

setName :: Text -> PlayerIdentity -> PlayerIdentity
setName name (PlayerIdentity attrs) = PlayerIdentity $ attrs {playerIdentityName = name}

setDeck :: Deck -> PlayerIdentity -> PlayerIdentity
setDeck deck (PlayerIdentity attrs) = PlayerIdentity $ attrs & deckL .~ deck'
 where
  ident = playerIdentityId attrs
  deck' =
    Deck $
      map
        (\c -> c {pcOwner = Just ident, pcController = Just ident})
        (unDeck deck)

lookupAlterEgo :: CardDef -> IdentityId -> Maybe PlayerIdentitySide
lookupAlterEgo cardDef ident =
  Just . AlterEgoSide $ lookupAlterEgoByCardCode (toCardCode cardDef) ident

lookupHero :: CardDef -> IdentityId -> Maybe PlayerIdentitySide
lookupHero cardDef ident =
  Just . HeroSide $ lookupHeroByCardCode (toCardCode cardDef) ident

-- | Changing to a form. An identity with more than two (Ant-Man's fold-out card) shows
-- the face on the choice, as its hero forms share a name.
formChoice :: PlayerIdentity -> Side -> Choice
formChoice p side
  | HashMap.size sides > 2, Just face <- HashMap.lookup side sides = CardLabel (sideCardCode face) (ChangeToForm side)
  | otherwise = ChangeToForm side
 where
  sides = playerIdentitySides (toAttrs p)
  sideCardCode = \case
    HeroSide x -> toCardCode x
    AlterEgoSide x -> toCardCode x

takeTurn :: (ReverseQueue m) => PlayerIdentity -> m ()
takeTurn attrs = do
  identityStep (toId attrs) PlayerTurnOption
  identityStep (toId attrs) CheckIfPassed

instance HasAbilities PlayerIdentity where
  getAbilities a =
    let
      sideAbilities = case currentIdentity a of
        HeroSide x -> getAbilities x
        AlterEgoSide x -> getAbilities x
     in
      [limitedAbility a 100 (PerTurn 1) Action (IsSelf <> SelfMatches IdentityCanChangeForm) NoCost ChangeForm]
        <> sideAbilities

isPlayable :: (HasQueue m, MonadRandom m, MonadThrow m, Projection m PlayerIdentity, HasGame m) => PlayerIdentity -> PlayerCard -> m Bool
isPlayable = isPlayableWith False

playableCardsWithReduction :: (ReverseQueue m) => IdentityId -> Natural -> CardMatcher -> m [PlayerCard]
playableCardsWithReduction you reduction matcher = do
  cards <- filter (cardMatch matcher) <$> projectMap PlayerIdentityHand unHand you
  filterM (canPlayCardWithReduction you reduction) cards

-- | Mysterio's encounter cards keep their encounter identity in a player's deck.
-- Deal each facedown and replace it with a draw after this batch finishes.
replacePlayerDeckEncounters :: (ReverseQueue m) => IdentityId -> [PlayerCard] -> m [PlayerCard]
replacePlayerDeckEncounters you cards = do
  let encounters = mapMaybe playerZoneEncounterCard (filter (not . cardMatch (CardWithTrait Shapeshifter)) cards)
  traverse_ (identityStep you . DealtEncounterCard) encounters
  unless (null encounters) $ drawCards you (fromIntegral $ length encounters)
  pure [card | card <- cards, isNothing (playerZoneEncounterCard card) || cardMatch (CardWithTrait Shapeshifter) card]

discardPlayerDeckEncounters :: ReverseQueue m => IdentityId -> [PlayerCard] -> m [PlayerCard]
discardPlayerDeckEncounters you cards = do
  ordinary <- replacePlayerDeckEncounters you cards
  let shapeshifters = filter (cardMatch $ CardWithTrait Shapeshifter) ordinary
  traverse_ discardCard shapeshifters
  pure $ filter (not . cardMatch (CardWithTrait Shapeshifter)) ordinary

-- | Whether the card can be played, its resource cost ignored if so ("play a card from
-- your hand, ignoring its resource cost").
isPlayableWith :: (HasQueue m, MonadRandom m, MonadThrow m, Projection m PlayerIdentity, HasGame m) => Bool -> PlayerIdentity -> PlayerCard -> m Bool
isPlayableWith ignoringCost attrs c = do
  effectiveDef <- effectivePlayerCardDef ident c
  affordable <- notNull <$> playableTargetsWith ignoringCost attrs c
  -- a card with a window is played as an action only if it says so, under its criteria
  passedCriteria <- passesCardCriteria ident c (fromMaybe (cdCriteria effectiveDef) (cdActionCriteria effectiveDef) <> toAdditionalCriteria effectiveDef)
  passedPlayLimits <- checkPlayLimits attrs c
  forbidden <- any (\case CannotPlayCards m -> cardMatch m c; _ -> False) <$> getModifiers (toId attrs)
  matchesUnique <-
    if cdUnique def
      then any (uniqueMatch (title $ cdName def, maybe [] pure (subtitle $ cdName def))) <$> getUniqueTitlesInPlay
      else pure False
  pure $
    affordable
      && passedCriteria
      && passedPlayLimits
      && not matchesUnique
      && not forbidden
      && (isNothing (cdResponseWindow def) || isJust (cdActionCriteria def))
      -- only these are played: not a resource, nor an obligation held in hand (System Shock)
      && (cdCardType def `elem` [AllyType, EventType, SupportType, UpgradeType, PlayerSideSchemeType])
 where
  ident = toId attrs
  def = getCardDef c

-- \^ this is critical and order matters to avoid infinite recursionl

-- | Whether two unique cards match (RR "Unique"): they share a title and neither has a
-- subtitle or alter-ego title, or a subtitle or alter-ego title of one is any name of the
-- other.
uniqueMatch :: (Text, [Text]) -> (Text, [Text]) -> Bool
uniqueMatch (t1, s1) (t2, s2) =
  (t1 == t2 && null s1 && null s2)
    || any (`elem` (t2 : s2)) s1
    || any (`elem` (t1 : s1)) s2

-- | The supports and upgrades with the trait the player controls ("Max 1 Team per
-- player").
controlledWithTrait :: (HasGame m) => Trait -> IdentityId -> m Natural
controlledWithTrait t p = do
  supports <- selectListCount (SupportMatches [SupportWithTrait t, SupportControlledBy $ IdentityWithId p])
  upgrades <- fromIntegral . length <$> selectList (UpgradeWithTrait t <> upgradeControlledBy p)
  pure $ supports + upgrades

checkPlayLimits :: (HasGame m, HasQueue m, MonadThrow m, Projection m PlayerIdentity) => PlayerIdentity -> PlayerCard -> m Bool
checkPlayLimits ident c = case cdLimit def of
  Nothing -> pure True
  Just limit -> case limit of
    MaxPerPlayer 1 -> case cdCardType def of
      UpgradeType -> selectNone $ UpgradeNamed name <> upgradeControlledBy (toId ident)
      SupportType -> selectNone $ SupportMatches [SupportNamed name, SupportControlledBy $ IdentityWithId (toId ident)]
      _ -> error "Unhandled"
    MaxPerPlayer _ -> error "Unhandled"
    -- any player without one may take it
    MaxPerPlayerUnderAnyControl n -> do
      players <- getPlayers
      flip anyM players \p -> case cdCardType def of
        SupportType -> (< n) <$> selectListCount (SupportMatches [SupportNamed name, SupportControlledBy $ IdentityWithId p])
        _ -> (< n) . fromIntegral . length <$> selectList (UpgradeNamed name <> upgradeControlledBy p)
    UnderAnyControl -> pure True
    MaxTraitPerPlayerUnderAnyControl t n -> anyM (fmap (< n) . controlledWithTrait t) =<< getPlayers
    MaxPerAlly 1 -> case cdCardType def of
      UpgradeType -> selectAny $ NotAlly $ AllyWithUpgrade $ UpgradeNamed name
      _ -> error "Unhandled"
    MaxPerAlly _ -> error "Unhandled"
    MaxPerEnemy 1 -> case cdCardType def of
      UpgradeType -> selectAny $ NotEnemy $ EnemyWithUpgrade $ UpgradeNamed name
      _ -> error "Unhandled"
    MaxPerEnemy _ -> error "Unhandled"
    MaxPerScheme n -> do
      targets <- fromMaybe [] <$> getPlayTargets (toId ident) c
      existing <- selectList $ UpgradeNamed name
      occupied <- concatMapM getUpgradeAttachedSchemes existing
      pure $ any (\target -> maybe False (\scheme -> count (== scheme) occupied < n) (targetScheme target)) targets
    -- copies played by any player count
    MaxPerWindow n -> (< n) . count ((== name) . cdName) <$> getPlayedThisWindow
    MaxPerRound n -> (< n) . count ((== name) . cdName . snd) <$> getPlayedThisRound
    MaxPerPhase n -> (< n) . count ((== name) . cdName . snd) <$> getPlayedThisPhase
    -- a friendly character without one: an ally, or an identity
    MaxPerCharacter 1 -> case cdCardType def of
      UpgradeType ->
        orM
          [ selectAny $ NotAlly $ AllyWithUpgrade $ UpgradeNamed name
          , anyM (\p -> selectNone $ UpgradeNamed name <> UpgradeOnIdentity (IdentityWithId p)) =<< getPlayers
          ]
      _ -> error "Unhandled"
    MaxPerCharacter _ -> error "Unhandled"
    MaxPerSideScheme 1 -> case cdCardType def of
      UpgradeType -> selectAny $ NotSideScheme $ SideSchemeWithUpgrade $ UpgradeNamed name
      _ -> error "Unhandled"
    MaxPerSideScheme _ -> error "Unhandled"
 where
  def = getCardDef c
  name = cdName def
  targetScheme = \case
    MainSchemeRef sid -> Just $ SchemeMainSchemeId sid
    SideSchemeRef sid -> Just $ SchemeSideSchemeId sid
    SchemeRef sid -> Just sid
    _ -> Nothing

-- | The resource cost of playing the card on the target, as modified: Iron Man's "reduce
-- the cost to play each upgrade on Iron Man" answers for the character it is played on.
getModifiedCost :: (HasGame m) => PlayerIdentity -> PlayerCard -> Maybe Target -> m Int
getModifiedCost attrs c mTarget =
  getModifiedCostWithBase attrs c mTarget $ case cdCost (getCardDef c) of
    Just (-1) -> Just (fromIntegral $ fst $ printedXParameters c)
    n -> n

-- | Modifiers still inspect the original card, whose printed cost remains X.
getModifiedCostWithBase :: (HasGame m) => PlayerIdentity -> PlayerCard -> Maybe Target -> Maybe Int -> m Int
getModifiedCostWithBase attrs c mTarget base = do
  (ignored, adjustment) <- resourceCostAdjustment attrs c mTarget
  pure $ if ignored then 0 else maybe 0 (max 0 . (+ adjustment)) base

resourceCostAdjustment :: (HasGame m) => PlayerIdentity -> PlayerCard -> Maybe Target -> m (Bool, Int)
resourceCostAdjustment attrs c mTarget = do
  modifiers <- (<>) <$> getModifiers attrs <*> getModifiers c
  onTarget <- maybe (pure []) getModifiers mTarget
  soldier <- if toCardCode c == "40023" then selectAny (IdentityWithId (toId attrs) <> IdentityWithTrait Soldier) else pure False
  let intrinsic = if soldier then -1 else 0
  pure (IgnoresResourceCost `elem` modifiers || IgnoresResourceCostForCard (pcCardId c) `elem` modifiers, intrinsic + sum (map change modifiers <> map changeOn onTarget))
 where
  change = \case
    ResourceCostReduction n -> negate (fromIntegral n)
    ResourceCostReductionForCard cid n | cid == pcCardId c -> negate (fromIntegral n)
    ResourceCostReductionFor matcher n | cardMatch matcher c -> negate (fromIntegral n)
    ResourceCostReductionForHandCard matcher n | cardMatch matcher c, c `elem` unHand (playerIdentityHand $ toAttrs attrs) -> negate (fromIntegral n)
    AdditionalResourceCost matcher n | cardMatch matcher c -> fromIntegral n
    _ -> 0
  changeOn = \case
    UpgradeOnThisCostReduction n | cdCardType (getCardDef c) == UpgradeType -> negate (fromIntegral n)
    _ -> 0

-- | The marker's bound matters only when ignoring resource costs, when arbitrarily
-- large values cannot improve the effect. Ordinary play permits all affordable X.
printedXParameters :: PlayerCard -> (Natural, UpgradeMatcher)
printedXParameters c = fromMaybe (0, UpgradeMatches []) $ headMay
  [(minimumX, matcher) | PrintedXCost minimumX matcher <- flattenCosts (cdAbilityCost $ getCardDef c)]

printedXChoices :: (HasQueue m, MonadRandom m, MonadThrow m, Projection m PlayerIdentity, HasGame m) => Bool -> PlayerIdentity -> PlayerCard -> Maybe Target -> m [Natural]
printedXChoices ignoringCost attrs c mTarget = do
  let ident = toId attrs
      (minimumX, meaningful) = printedXParameters c
      allowed = canSpendOn (cdCriteria $ getCardDef c)
  payers <- resourceCostPayers ident (Just c)
  resources <- filter allowed <$> concatMapM (\payer -> do
    let reserved = if payer == toId attrs then sum [n | IdentityCountersCost n <- flattenCosts (cdAbilityCost $ getCardDef c)] else 0
    abilities <- concatMapM (abilityResourcesWithReservedCounters reserved) =<< getPaymentResourceAbilities (toId attrs) payer (Just c)
    cards <- filter (\candidate -> pcCardId candidate /= pcCardId c && not (isChosenHandTarget c mTarget candidate)) <$> getPaymentSourcesFor (toId attrs) payer (Just c)
    doubleWildResources (Just $ getCardDef c) . (<> abilities) <$> concatMapM (\card -> resourcesFor card (Just c)) cards) payers
  (ignored, adjustment) <- resourceCostAdjustment attrs c mTarget
  modifiers <- getModifiers attrs
  inHand <- projectP PlayerIdentityHand (elem c . unHand) ident
  let potentialReduction = if inHand then sum [fromIntegral n | MayReduceCostToPlay n <- modifiers] else 0
      upperPaid = max 0 (length resources - adjustment + potentialReduction)
  upperEffect <- fromIntegral <$> selectListCount meaningful
  let upper = if ignoringCost || ignored then upperEffect else fromIntegral upperPaid
  pure [minimumX .. upper]

-- | The whole cost of playing the card on the target: its resource cost, and the cost
-- of its ability (an event's "→"), paid together.
getCardCost ::
  (HasQueue m, MonadThrow m, Projection m PlayerIdentity, HasGame m) =>
  PlayerIdentity ->
  PlayerCard ->
  Maybe Target ->
  m Cost
getCardCost = getCardCostWithX Nothing

getCardCostWithX :: (HasQueue m, MonadThrow m, Projection m PlayerIdentity, HasGame m) => Maybe Natural -> PlayerIdentity -> PlayerCard -> Maybe Target -> m Cost
getCardCostWithX chosenX attrs c mTarget = do
  modifiedCost <- case chosenX of
    Nothing -> getModifiedCost attrs c mTarget
    Just n -> getModifiedCostWithBase attrs c mTarget (Just $ fromIntegral n)
  printed <- case (cdAbilityCost def, mTarget) of
    (abilityCost, Just (CardIdRef cid)) | usesTargetCost abilityCost -> do
      cards <- concatMapM (projectMap PlayerIdentityDiscard unDiscard) =<< getPlayers
      pure $ maybe 0 (fromMaybe 0 . cdCost . getCardDef) (find ((== cid) . pcCardId) cards)
    _ -> pure 0
  pure $
    mconcat (map (ResourceCost . (\r -> r <|> spendOnly (cdCriteria def))) (requirementResourceCost (requiredResources def) modifiedCost))
      <> mconcat (replicate printed (ResourceCost Nothing))
      <> filterCosts (\case TargetPrintedCost -> False; PrintedXCost _ _ -> False; _ -> True) (cdAbilityCost def)
 where
  def = getCardDef c
  usesTargetCost = elem TargetPrintedCost . flattenCosts

-- | The targets the card can be played on whose cost the player can pay: just
-- @Nothing@ for a card played on nothing.
playableTargets ::
  (HasQueue m, MonadRandom m, MonadThrow m, Projection m PlayerIdentity, HasGame m) =>
  PlayerIdentity ->
  PlayerCard ->
  m [Maybe Target]
playableTargets = playableTargetsWith False

-- | 'playableTargets', the resource cost ignored if so: its other costs still due.
playableTargetsWith ::
  (HasQueue m, MonadRandom m, MonadThrow m, Projection m PlayerIdentity, HasGame m) =>
  Bool ->
  PlayerIdentity ->
  PlayerCard ->
  m [Maybe Target]
playableTargetsWith ignoringCost attrs c = do
  targets <- maybe [Nothing] (map Just) <$> getPlayTargets (toId attrs) c
  flip filterM targets \mTarget -> do
    cost <- getCardCost attrs c mTarget
    payers <- resourceCostPayers (toId attrs) (Just c)
    resources <- filter (canSpendOn $ cdCriteria $ getCardDef c) <$> concatMapM (\payer -> do
      let reserved = if payer == toId attrs then sum [n | IdentityCountersCost n <- flattenCosts cost] else 0
      abilities <- concatMapM (abilityResourcesWithReservedCounters reserved) =<< getPaymentResourceAbilities (toId attrs) payer (Just c)
      cards <- filter (\candidate -> pcCardId candidate /= pcCardId c && not (isChosenHandTarget c mTarget candidate)) <$> getPaymentSourcesFor (toId attrs) payer (Just c)
      doubleWildResources (Just $ getCardDef c) . (<> abilities) <$> concatMapM (\payingCard -> resourcesFor payingCard (Just c)) cards) payers
    prePlayReduction <- mayReduceCostToPlay attrs c mTarget
    spendingReduction <- continuousSpendingCostReduction attrs c mTarget payers
    let reduction = prePlayReduction + spendingReduction
    (ignored, _) <- resourceCostAdjustment attrs c mTarget
    printedResourceCost <- getModifiedCost attrs c mTarget
    let requirements = requiredResources c
        requirementAllowed = null requirements || not (ignoringCost || ignored)
        cardSymbols = requirementResourceCost requirements printedResourceCost
        reducedCardSymbols = map (\r -> r <|> spendOnly (cdCriteria $ getCardDef c))
          (requirementResourceCost requirements (max 0 (printedResourceCost - reduction)))
        payableSymbols = reducedCardSymbols <> drop (length cardSymbols) (costResources cost)
    affordable <- if not requirementAllowed then pure False
      else if cdCost (getCardDef c) == Just (-1)
      then notNull <$> printedXChoices ignoringCost attrs c mTarget
      else pure (ignoringCost || if null requirements
        then length resources + reduction >= length (costResources cost)
        else coversResources payableSymbols resources)
    handTarget <- if ChosenHandCardCost `elem` flattenCosts cost
      then projectP PlayerIdentityHand (any (isChosenHandTarget c mTarget) . unHand) (toId attrs)
      else pure True
    otherPayers <- allianceCostPayers (toId attrs) (Just c)
    ((affordable && handTarget) &&) <$> canAffordCardOtherCosts (toId attrs) otherPayers c (otherCosts cost)

-- | Only cards with a continuously evaluated resource cost consider counter
-- generation while spending. Ordinary card costs retain their standard timing.
continuousSpendingCostReduction :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m, Projection m PlayerIdentity) => PlayerIdentity -> PlayerCard -> Maybe Target -> [IdentityId] -> m Int
continuousSpendingCostReduction attrs card target payers = do
  continuous <- elem ContinuouslyRecalculateResourceCost <$> getModifiers card
  if not continuous then pure 0 else do
    gains <- concatMapM (\payer -> do
      candidates <- filter (\candidate -> pcCardId candidate /= pcCardId card && not (isChosenHandTarget card target candidate)) <$> getPaymentSourcesFor (toId attrs) payer (Just card)
      spendable <- filterM (fmap (any $ canSpendOn $ cdCriteria $ getCardDef card) . (`resourcesFor` Just card)) candidates
      concatMapM (spendingInterruptCounters payer) spendable) payers
    if null gains then pure 0 else do
      current <- getModifiedCost attrs card target
      prospective <- withAdditionalIdentityCounters gains $ getModifiedCost attrs card target
      pure $ max 0 (current - prospective)

-- | The hand card already selected to pay a choose-card cost is logically reserved
-- from resource spending, while remaining in its actual zone until the effect.
isChosenHandTarget :: PlayerCard -> Maybe Target -> PlayerCard -> Bool
isChosenHandTarget source target candidate =
  ChosenHandCardCost `elem` flattenCosts (cdAbilityCost $ getCardDef source)
    && pcCardId source /= pcCardId candidate
    && target == Just (CardIdRef $ pcCardId candidate)

-- | Resource abilities resolve before the other parts of a card's cost. A ready
-- Gauntlet Gun can therefore supply ammo needed by the event it is paying for.
-- Only the actual currently affordable resource abilities are considered; their
-- own costs still use ordinary affordability, avoiding recursive speculation.
canAffordCardOtherCosts :: (HasQueue m, MonadRandom m, MonadThrow m, Projection m PlayerIdentity, HasGame m) => IdentityId -> [IdentityId] -> PlayerCard -> Cost -> m Bool
canAffordCardOtherCosts ident payers card cost = do
  current <- project PlayerIdentityCounters ident
  bonus <- if current >= needed
    then pure 0
    else do
      abilities <- getResourceAbilities ident (Just card)
      amounts <- for abilities \a -> do
        modifiers <- getModifiers (abilitySource a)
        pure $ sum [n | MayGenerateIdentityCountersFor matcher n <- modifiers, cardMatch matcher card]
      pure $ sum amounts
  rest <- canAffordAllianceCost payers (toSource card) (filterCosts (\case IdentityCountersCost _ -> False; _ -> True) cost)
  meaningful <- eventExhaustionsAffordable ident payers card cost
  pure $ current + bonus >= needed && rest && meaningful
 where needed = sum [n | IdentityCountersCost n <- flattenCosts cost]

-- | How much less a card from hand could cost through an interrupt as it is played
-- ('MayReduceCostToPlay'), no more than its resource cost.
mayReduceCostToPlay :: (HasGame m) => PlayerIdentity -> PlayerCard -> Maybe Target -> m Int
mayReduceCostToPlay attrs c mTarget
  | c `notElem` unHand (playerIdentityHand $ toAttrs attrs) = pure 0
  | otherwise = do
      reductions <- sum . map fromIntegral . (\ms -> [n | MayReduceCostToPlay n <- ms]) <$> getModifiers attrs
      min reductions <$> getModifiedCost attrs c mTarget

getPlayableCards :: (HasQueue m, MonadRandom m, MonadThrow m, Projection m PlayerIdentity, HasGame m) => PlayerIdentity -> m [PlayerCard]
getPlayableCards player = do
  held <- getHeldPlayableCards (toId player)
  filterM (isPlayable player) (cards <> held)
 where
  attrs = toAttrs player
  cards =
    unHand (playerIdentityHand attrs)
      <> filter (cdPlayableFromDiscard . getCardDef) (unDiscard $ playerIdentityDiscard attrs)

getChoices ::
  (MonadThrow m, MonadRandom m, HasQueue m, HasGame m, Projection m PlayerIdentity) => PlayerIdentity -> m [Choice]
getChoices attrs = do
  let ident = toId attrs
  abilities <- getAbilitiesFor ident
  playableCards <- getPlayableCards attrs
  validAbilities <-
    filterM
      ( andM
          . sequence
            [ withinUseLimit ident
            , passesCriteria ident
            , passesCanAffordCost ident
            , pure . passesTiming ident
            , passesTypeIsRelevant ident
            , triggerable
            ]
      )
      abilities
  pure $
    map UseAbility validAbilities
      <> map (($ Nothing) . PlayCard) playableCards

-- | An "Alter-Ego Action" a card forbids to be triggered is not.
triggerable :: (HasGame m) => Ability -> m Bool
triggerable a = do
  modifiers <- getModifiers (abilitySource a)
  let blockedDiscard = CannotBeDiscarded `elem` modifiers
        && abilityChoices a == [DiscardTarget (toTarget $ abilitySource a)]
      blockedForm = abilityType a == AlterEgoAction && CannotTriggerAlterEgoActions `elem` modifiers
  pure $ not (blockedDiscard || blockedForm)

getObligations :: (MonadRandom m) => PlayerIdentity -> m [EncounterCard]
getObligations attrs = case currentIdentity attrs of
  HeroSide _ -> error "Can not call via hero side"
  AlterEgoSide side ->
    traverse genEncounterCard (alterEgoObligations $ toAttrs side)

-- | The obligations of the identity's alter-egos.
identityObligations :: PlayerIdentity -> [CardDef]
identityObligations = concatMap obligationsOf . HashMap.elems . playerIdentitySides . toAttrs
 where
  obligationsOf = \case
    AlterEgoSide side -> alterEgoObligations (toAttrs side)
    HeroSide _ -> []

-- | The identity's keywords, with those its modifiers give it.
getModifiedKeywords :: (HasGame m) => PlayerIdentity -> m [Keyword]
getModifiedKeywords pid = do
  modifiers <- getModifiers pid
  pure $ toList (cdKeywords $ getCardDef pid) <> [k | KeywordModifier k <- modifiers]

-- | Reserve the played card and retain chosen X independently of generated resources.
beginCardPayment :: (ReverseQueue m) => Maybe Natural -> PlayerIdentity -> PlayerCard -> Maybe W.Window -> Maybe Target -> m PlayerIdentity
beginCardPayment chosenX pid card mWindow mTarget = do
  cost <- getCardCostWithX chosenX pid card mTarget
  activeCostId <- getRandom
  holder <- getAllyHoldingCard card
  supportHolder <- getSupportHoldingCard card
  for_ holder (\aid -> reserveAllyHeldCard aid card)
  for_ supportHolder (\sid -> supportStep sid (ReserveHeldCard card))
  setActiveCost $
      ActiveCost
        activeCostId
        (toId pid)
        (ForCard card)
        cost
        (maybe NoPayment XPayment chosenX <> maybe NoPayment TargetPayment mTarget <> maybe NoPayment (\aid -> ReservedAllyCardPayment aid card) holder <> maybe NoPayment (\sid -> ReservedSupportCardPayment sid card) supportHolder)
        mWindow
        mempty
        Nothing
  pure $ PlayerIdentity $ toAttrs pid & (handL %~ Hand . filter (/= card) . unHand)

runIdentityMessage ::
  (ReverseQueue m) =>
  IdentityMessage ->
  PlayerIdentity ->
  m PlayerIdentity
runIdentityMessage msg pid@(PlayerIdentity attrs@PlayerIdentityAttrs {..}) =
  case msg of
    SwapIdentityVersion heroCode alterCode ->
      pure $ PlayerIdentity attrs
        { playerIdentitySides = fromList
            [(A, HeroSide $ lookupHeroByCardCode heroCode playerIdentityId)
            ,(B, AlterEgoSide $ lookupAlterEgoByCardCode alterCode playerIdentityId)]
        }
    SetUpWeatherDeck cards -> do
      shuffled <- shuffleM cards
      unless (null shuffled) $ chooseOne playerIdentityId
        [cardLabeled (toCardCode card) $ identityStep playerIdentityId (ChooseWeatherSupport card False) | card <- shuffled]
      pure $ PlayerIdentity attrs {playerIdentityWeatherDeck = Just shuffled}
    ChooseWeatherSupport card resolveSpecial
      | Just cards <- playerIdentityWeatherDeck, card `elem` cards -> do
          current <- selectOne (SupportMatches [SupportWithTrait Weather, SupportControlledBy $ IdentityWithId playerIdentityId])
          old <- for current $ \sid -> getsGame $ \g -> case cardInPlay g (SupportRef sid) of
            Just (PlayerCard c) -> Just c
            _ -> Nothing
          swapWeatherSupport playerIdentityId current card resolveSpecial
          let replacement = join old
              cards' = concatMap (\c -> if c == card then maybe [] pure replacement else [c]) cards
          pure $ PlayerIdentity attrs {playerIdentityWeatherDeck = Just cards'}
    ChooseWeatherSupport {} -> pure pid
    SetupIdentity -> do
      let nemesisSet = getNemesisSet $ toCardCode pid
      obligations <- getObligations pid
      nemesisSetCards <- gatherEncounterSet nemesisSet
      setAside (EncounterCard <$> nemesisSetCards)
      shuffleIntoEncounterDeck obligations
      -- permanent cards are set aside before setup, for other cards to put into play
      -- (RR Permanent); those with setup begin the game in play instead
      let (permanent, deck) = partition (\c -> let ks = cdKeywords (getCardDef c) in Permanent `elem` ks && Keyword.Setup `notElem` ks) (unDeck playerIdentityDeck)
      setAside (PlayerCard <$> permanent)
      shuffleDeck playerIdentityId
      pure $ PlayerIdentity $ attrs & deckL .~ Deck deck
    -- the setup keyword: "begins the game in play"
    PutSetupCardsIntoPlay -> do
      for_ (filter (elem Keyword.Setup . cdKeywords . getCardDef) $ unDeck playerIdentityDeck) (putIntoPlay playerIdentityId)
      pure pid
    -- draw up to hand size, mulligan any of it, draw again, then the setup abilities
    DealStartingHand -> do
      campaign <- getCampaign
      let extraMulligan = maybe False (\c -> c.kind == Campaign.SinisterMotives && c.campaignLog.sinister.reputation >= 5) campaign
      let setupAbilities = filter ((== Setup) . abilityType) $ getAbilities pid
      for_ [DrawOrDiscardToHandLimit, DiscardCards, DrawOrDiscardToHandLimit] (identityStep playerIdentityId)
      when extraMulligan $
        for_ [DiscardCards, DrawOrDiscardToHandLimit] (identityStep playerIdentityId)
      for_ setupAbilities \a -> ranAbility (toId pid) (toTarget pid) (abilityIndex a) [] NoPayment
      pure pid
    IdentityDealtBoost card -> pure $ PlayerIdentity attrs
      {playerIdentityBoostCards = playerIdentityBoostCards <> [card]}
    EncounterCardInDiscard card -> pure $ PlayerIdentity $ attrs
      & discardL %~ Discard . (encounterCardInPlayerDeck playerIdentityId card :) . unDiscard
    MoveIdentityBoostCardsToVillain vid -> do
      for_ playerIdentityBoostCards (villainStep vid . VillainDealtBoost)
      pure $ PlayerIdentity attrs {playerIdentityBoostCards = []}
    BeginTurn -> do
      -- before anything else is done in the turn
      checkWindows [W.Window W.When $ W.TurnBegan playerIdentityId]
      checkWindows [W.Window W.After $ W.TurnBegan playerIdentityId]
      takeTurn pid
      pure $ PlayerIdentity $ attrs & passedL .~ False
    CheckIfPassed -> do
      unless playerIdentityPassed (takeTurn pid)
      pure pid
    PlayerTurnOption -> do
      choices <- getChoices pid
      chooseOnePrompted playerIdentityId "$label.yourTurn" $ map pure (EndTurn : choices)
      pure pid
    -- "when your turn ends" is answered before it does
    EndedTurn -> do
      checkWindows [W.Window W.When $ W.TurnEnded playerIdentityId]
      identityEndedTurn playerIdentityId
      -- "after your turn ends" (Sap Power)
      checkWindows [W.Window W.After $ W.TurnEnded playerIdentityId]
      pure $ PlayerIdentity $ attrs & passedL .~ True
    ShuffleDeck -> do
      deck <- shuffleM (unDeck $ attrs ^. deckL)
      pure $ PlayerIdentity $ attrs & deckL .~ Deck deck
    ShuffleIdentityDiscardBackIntoDeck -> do
      deck' <-
        shuffleM
          (unDeck (attrs ^. deckL) <> unDiscard (attrs ^. discardL))
      pure $
        PlayerIdentity $
          attrs
            & (deckL .~ Deck deck')
            & (discardL .~ Discard [])
    ShuffleIntoIdentityDeck cards -> do
      deck <- shuffleM (unDeck (attrs ^. deckL) <> cards)
      -- a card shuffled in from the discard pile leaves it
      pure $ PlayerIdentity $ attrs & deckL .~ Deck deck & discardL %~ Discard . filter (`notElem` cards) . unDiscard
    DrawOrDiscardToHandLimit -> do
      modifiedHandSize <- getModifiedHandSize pid
      let
        diff =
          fromIntegral modifiedHandSize - countedHandSize (unHand playerIdentityHand)
      when (diff > 0 && (notNull (unDeck playerIdentityDeck) || notNull (unDiscard playerIdentityDiscard))) $ do
        drawCards playerIdentityId 1
        identityStep playerIdentityId DrawToHandLimit
      when (diff < 0) $
        chooseOnePrompted
          playerIdentityId
          "$label.discardToHandSize"
          [ targeting c do
            discardCard c
            identityStep playerIdentityId DrawOrDiscardToHandLimit
          | c <- choosableToDiscard (filter (not . cdDoesNotCountTowardHandSize . getCardDef) $ unHand playerIdentityHand)
          ]
      pure pid
    DrawToHandLimit -> do
      modifiedHandSize <- getModifiedHandSize pid
      let
        diff =
          fromIntegral modifiedHandSize - countedHandSize (unHand playerIdentityHand)
      when (diff > 0 && (notNull (unDeck playerIdentityDeck) || notNull (unDiscard playerIdentityDiscard))) $ do
        drawCards playerIdentityId 1
        identityStep playerIdentityId DrawToHandLimit
      pure pid
    DrawToPrintedHandSize -> do
      let short = fromIntegral (unHandSize $ handSize pid) - countedHandSize (unHand playerIdentityHand)
      when (short > 0 && (notNull (unDeck playerIdentityDeck) || notNull (unDiscard playerIdentityDiscard))) $ do
        drawCards playerIdentityId 1
        identityStep playerIdentityId DrawToPrintedHandSize
      pure pid
    DrawCards fromZone n -> case fromZone of
      FromHand -> error "Impossible"
      FromDiscard -> error "Impossible"
      RandomFromHand -> error "Impossible"
      FromEncounterDeck -> error "Impossible"
      FromDeck -> do
        let
          (cards, deck) = splitAt (fromIntegral n) (unDeck playerIdentityDeck)
          short = subtractNatural (fromIntegral $ length cards) n
        -- the deck emptying is made again at once (see 'RunMessage PlayerIdentity'),
        -- and the drawing carries on from it
        when (short > 0 && notNull (unDiscard playerIdentityDiscard)) $
          drawCards playerIdentityId short
        -- an obligation drawn goes into play in their play area, not drawn again for
        -- (System Shock, whose text only works in hand, stays there)
        ordinary <- replacePlayerDeckEncounters playerIdentityId cards
        for_ (filter (cardMatch $ CardWithTrait Shapeshifter) ordinary) (\card -> cardSpecific GameRef "mgShapeshifterEnteredHand" (playerIdentityId,card))
        let (obligations, drawn) = partition (\c -> cdCardType (getCardDef c) == ObligationType && not (staysInHand c)) ordinary
        for_ obligations (putObligationIntoPlay playerIdentityId)
        pure $
          PlayerIdentity $
            attrs
              & handL
                %~ Hand
                  . (<> drawn)
                  . unHand
              & deckL
                .~ Deck deck
    ReadyCards -> do
      schemes <- getControlledPlayerSideSchemes playerIdentityId False
      for_ schemes (\sid -> sideSchemeStep sid ReadiedSideScheme)
      for_ (HashSet.toList playerIdentityAllies) readyByGame
      for_ (HashSet.toList playerIdentitySupports) ready
      for_ (HashSet.toList playerIdentityUpgrades) ready
      runIdentityMessage ReadiedIdentityByGame pid
    -- an identity that cannot change form stays as it is, whatever would change it
    ChooseOtherForm -> do
      canChange <- identityMatches IdentityCanChangeForm playerIdentityId
      let
        otherForms = filter (/= playerIdentitySide) $ HashMap.keys playerIdentitySides
      when canChange $ chooseOrRunOne playerIdentityId $ map (pure . formChoice pid) otherForms
      pure pid
    ChooseHeroForm -> do
      canChange <- identityMatches IdentityCanChangeForm playerIdentityId
      let
        heroForms = [s | (s, HeroSide _) <- HashMap.toList playerIdentitySides, s /= playerIdentitySide]
      when canChange $ chooseOrRunOne playerIdentityId $ map (pure . formChoice pid) (sortOn show heroForms)
      pure pid
    ChangedToForm side -> do
      modifiers <- getModifiers pid
      let toHero = case HashMap.lookup side playerIdentitySides of Just (HeroSide _) -> True; _ -> False
          canChange = CannotChangeForm `notElem` modifiers && (not toHero || CannotChangeToHeroForm `notElem` modifiers)
      if canChange
        then do
          when (side /= playerIdentitySide) $ transferMojoThreat (IdentityCharacter playerIdentityId)
          oldTraits <- getTraits pid
          let fromAlterEgo = case currentIdentity pid of AlterEgoSide _ -> True; _ -> False
          checkWindows $ W.Window W.After (W.IdentityChangesForm playerIdentityId)
            : [W.Window W.After (W.MutantChangedToHero playerIdentityId) | fromAlterEgo && toHero && Mutant `member` oldTraits]
          pure $ PlayerIdentity $ attrs & sideL .~ side
        else pure pid
    PlayingCard card mWindow -> do
      targets <- playableTargets pid card
      -- "when you play a card from your hand" is answered before its cost is worked out
      let fromHand = card `elem` unHand playerIdentityHand
          payFor mTarget =
            if cdCost (getCardDef card) == Just (-1)
              then do
                values <- printedXChoices False pid card mTarget
                unless (null values) $ chooseOnePromptedAbout playerIdentityId "$label.choosePrintedX" card
                  [labeled (countLabel "chooseXAmount" n) (cardSpecific pid "playChosenX" (card, mWindow, mTarget, n)) | n <- values]
              else do
                costly <- (> 0) <$> getModifiedCost pid card mTarget
                when (fromHand && costly) $
                  checkWindows [W.Window W.When $ W.PlayingCardFromHand playerIdentityId card]
                identityStep playerIdentityId $ PayForCard card mWindow mTarget
          underAnyControl = case cdLimit (getCardDef card) of
            Just (MaxPerPlayerUnderAnyControl _) -> True
            Just (MaxTraitPerPlayerUnderAnyControl _ _) -> True
            Just UnderAnyControl -> True
            _ -> False
          isMainScheme = \case
            MainSchemeRef _ -> True
            SchemeRef (SchemeMainSchemeId _) -> True
            _ -> False
      case targets of
        -- a card played on something is played on the one the player picks, even of one
        [Nothing] -> payFor Nothing
        -- whose control a card is played under, with only one player to take it, is not
        -- asked (left for the owner to decide)
        [mTarget] | underAnyControl -> payFor mTarget
        -- "attach to the main scheme": there is only ever one, so nothing to pick
        [Just t] | isMainScheme t -> payFor (Just t)
        _ -> chooseOnePromptedAbout playerIdentityId "$label.playOn" card [targeting t (payFor (Just t)) | Just t <- targets]
      pure pid
    -- a card played from a discard pile is left there until it is played, so that
    -- giving it up leaves it where it was
    PayForCard card mWindow mTarget -> beginCardPayment Nothing pid card mWindow mTarget
    PlayedCard {} -> pure pid
    PaidWithCard card -> do
      mActiveCost <- getActiveCost
      case mActiveCost of
        Nothing -> error "no cost"
        Just activeCostId -> do
          mCost <- getActiveCostCard activeCostId
          generated <- resourcesFor card mCost
          printed <- printedResourcesFor card
          spendCardWithResources activeCostId card generated printed
          pure $
            PlayerIdentity $
              attrs
                & (handL %~ Hand . filter (/= card) . unHand)
    -- & (discardL %~ Discard . (card :) . unDiscard)
    AllyCreated allyId -> do
      identityStep playerIdentityId CheckAllyLimit
      pure $ PlayerIdentity $ attrs & alliesL %~ HashSet.insert allyId
    CheckAllyLimit -> do
      limit <- fromIntegral <$> getModifiedAllyLimit pid
      counted <- filterM (fmap (notElem NotCountedAgainstAllyLimit) . getModifiers) (toList playerIdentityAllies)
      when (length counted > limit) $
        chooseOnePrompted
          playerIdentityId
          "$label.discardAnAlly"
          -- one not counted against the limit (Odin) does not bring them within it
          [targeting aid (removeFromPlay aid >> identityStep playerIdentityId CheckAllyLimit) | aid <- counted]
      pure pid
    UpgradeCreated upgradeId -> do
      identityStep playerIdentityId CheckRestrictedLimit
      pure $ PlayerIdentity $ attrs & upgradesL %~ HashSet.insert upgradeId
    UpgradeControlLost upgradeId -> do
      when (HashSet.member upgradeId playerIdentityUpgrades && not playerIdentityDefeated) do
        when (playerIdentityDamage > 0) do
          hp <- getModifiedHp pid
          when (playerIdentityDamage >= hp) $ defeatIdentity playerIdentityId
        identityStep playerIdentityId CheckRestrictedLimit
      pure $ PlayerIdentity $ attrs & upgradesL %~ HashSet.delete upgradeId
    -- two restricted upgrades, and those its modifiers add (any, or Weapons only); over
    -- that the player discards one they choose, until what remains fits
    CheckRestrictedLimit -> do
      (anyLimit, weaponLimit) <- getModifiedRestrictedLimits pid
      let restrictedMatcher = UpgradeWithKeyword Restricted <> upgradeControlledBy playerIdentityId
      restricted <- selectList restrictedMatcher
      weapons <- selectList $ restrictedMatcher <> UpgradeWithTrait Weapon
      let others = filter (`notElem` weapons) restricted
      let
        discardOf us = do
          discardable <- filterM (fmap not . upgradeMatches (UpgradeWithKeyword Permanent)) us
          chooseOnePrompted
            playerIdentityId
            "$label.discardARestrictedCard"
            [targeting u (removeFromPlay u >> identityStep playerIdentityId CheckRestrictedLimit) | u <- discardable]
      -- the Weapon-only slots cannot hold the others
      if
        | length others > fromIntegral anyLimit -> discardOf others
        | length restricted > fromIntegral (anyLimit + weaponLimit) -> discardOf restricted
        | otherwise -> pure ()
      pure pid
    AllyRemoved allyId -> do
      pure $ PlayerIdentity $ attrs & alliesL %~ HashSet.delete allyId
    MinionEngaged minionId -> do
      pure $ PlayerIdentity $ attrs & minionsL %~ HashSet.insert minionId
    MinionDisengaged minionId -> do
      pure $ PlayerIdentity $ attrs & minionsL %~ HashSet.delete minionId
    SupportCreated supportId -> do
      pure $ PlayerIdentity $ attrs & supportsL %~ HashSet.insert supportId
    SupportRemoved supportId -> do
      pure $ PlayerIdentity $ attrs & supportsL %~ HashSet.delete supportId
    AddToHand card -> do
      when (cardMatch (CardWithTrait Shapeshifter) card) $ cardSpecific GameRef "mgShapeshifterEnteredHand" (playerIdentityId,card)
      pure $
        PlayerIdentity $
          attrs
            & (handL %~ Hand . (card :) . unHand)
            & (deckL %~ Deck . filter (/= card) . unDeck)
            & (discardL %~ Discard . filter (/= card) . unDiscard)
    ChooseFromDiscard target choiceRules chooseMin chooseMax -> do
      focusing (PlayerCard <$> unDiscard playerIdentityDiscard) $
        identityStep playerIdentityId $ ChosenFromDiscard target choiceRules chooseMin chooseMax []
      pure pid
    ChosenFromDiscard target choiceRules chooseMin chooseMax cards -> do
      let
        discards = unDiscard playerIdentityDiscard
        focusedCards = filter (`notElem` cards) discards
        chosenNames = map (cdName . pcCardDef) cards
        choices = case choiceRules of
          DifferentCards ->
            filter ((`notElem` chosenNames) . cdName . pcCardDef) discards
          AnyCards -> focusedCards

      if length cards >= fromIntegral chooseMax
        then withChosen target FromDiscard $ PlayerCard <$> cards
        else do
          focusCards $ PlayerCard <$> focusedCards
          chooseOne playerIdentityId $
            [ labeled "$label.done" (withChosen target FromDiscard $ PlayerCard <$> cards)
            | length cards >= fromIntegral chooseMin
            ]
              <> [ targeting c $
                    identityStep playerIdentityId $
                      ChosenFromDiscard target choiceRules chooseMin chooseMax (c : cards)
                 | c <- choices
                 ]
      pure pid
    DiscardFor _ FromDeck _ _ -> error "Unhandled"
    DiscardedFor _ FromDeck _ _ _ -> error "Unhandled"
    DiscardFor _ FromEncounterDeck _ _ -> error "Unhandled"
    DiscardedFor _ FromEncounterDeck _ _ _ -> error "Unhandled"
    DiscardFor _ FromDiscard _ _ -> error "Unhandled"
    DiscardedFor _ FromDiscard _ _ _ -> error "Unhandled"
    DiscardFor target FromHand discardMin discardMax -> do
      identityStep playerIdentityId $ DiscardedFor target FromHand discardMin discardMax []
      pure pid
    DiscardedFor target FromHand discardMin discardMax cards -> do
      if length cards >= fromIntegral discardMax
        then withDiscarded target FromHand cards
        else
          chooseOne playerIdentityId $
            [ labeled "$label.done" (withDiscarded target FromHand cards)
            | length cards >= fromIntegral discardMin
            ]
              <> [ targeting c do
                    discardCard c
                    identityStep playerIdentityId $
                      DiscardedFor target FromHand discardMin discardMax (PlayerCard c : cards)
                 | c <- choosableToDiscard (unHand playerIdentityHand)
                 ]
      pure pid
    DiscardFor target RandomFromHand discardMin _ -> do
      let handCards = unHand playerIdentityHand
      discards <- take (fromIntegral discardMin) <$> shuffleM handCards
      for_ discards discardCard
      withDiscarded target RandomFromHand $ PlayerCard <$> discards
      pure pid
    DiscardedFor _ RandomFromHand _ _ _ -> error "Can not be called"
    DiscardCards -> do
      unless (null $ unHand playerIdentityHand) $ do
        chooseOnePrompted playerIdentityId "$label.discardAnyCards" $
          labeled "$label.doneDiscarding" (pure ())
            : [ targeting c do
                  discardCard c
                  identityStep playerIdentityId DiscardCards
              | c <- choosableToDiscard (unHand playerIdentityHand)
              ]
      pure pid
    DiscardFrom fromZone n mTarget -> case fromZone of
      FromHand -> do
        identityStep playerIdentityId $ DiscardedFor (fromMaybe GameRef mTarget) FromHand n n []
        pure pid
      FromDiscard -> error "Unhandled"
      FromEncounterDeck -> error "Unhandled"
      RandomFromHand -> do
        discards <-
          take (fromIntegral n)
            <$> shuffleM (unHand playerIdentityHand)
        for_ mTarget \target -> withDiscarded target fromZone $ PlayerCard <$> discards
        traverse_ discardCard (filter (cardMatch $ CardWithTrait Shapeshifter) discards)
        pure $
          PlayerIdentity $
            attrs
              & (discardL %~ Discard . (filter (not . cardMatch (CardWithTrait Shapeshifter)) discards <>) . unDiscard)
              & (handL %~ Hand . filter (`notElem` discards) . unHand)
      FromDeck -> do
        let
          (cards, deck') = splitAt (fromIntegral n) $ unDeck playerIdentityDeck
        discarded <- discardPlayerDeckEncounters playerIdentityId cards
        actor <- getYou
        for_ discarded $ \card -> checkWindows [W.Window W.After $ W.DiscardedFromPlayerDeck playerIdentityId [card], W.Window W.After $ W.PlayerDiscardedFromDeck actor playerIdentityId card]
        for_ mTarget \target -> withDiscarded target fromZone $ PlayerCard <$> discarded
        pure $
          PlayerIdentity $
            attrs
              & (discardL %~ Discard . (discarded <>) . unDiscard)
              & (deckL .~ Deck deck')
    -- "heroes cannot be readied by player card effects" (Unnatural Storm)
    ReadiedIdentity -> do
      storm <- elem CannotBeReadiedByPlayerCards <$> getModifiers pid
      if storm && isHero pid then pure pid else runIdentityMessage ReadiedIdentityByGame pid
    ReadiedIdentityByGame -> do
      allowed <- notElem CannotReady <$> getModifiers pid
      readies <- if allowed && playerIdentityExhausted then payAdditionalReadyCost playerIdentityId (toRef pid) else pure False
      when readies $ checkWindows [W.Window W.After $ W.IdentityReadied playerIdentityId]
      pure $ PlayerIdentity $ attrs & exhaustedL %~ (&& not readies)
    ExhaustedIdentity -> pure $ PlayerIdentity $ attrs & exhaustedL .~ True
    -- the active villain of the player's area, if it has one, then their minions
    VillainAndMinionsActivate -> do
      villain <- selectOne ActiveVillain
      case villain of
        Just vid -> do
          checkWindows [W.Window W.Would $ W.VillainActivationWouldBegin vid playerIdentityId]
          cardSpecific GameRef "resolveCurrentVillainActivation" playerIdentityId
        Nothing -> cardSpecific GameRef "resolveCurrentVillainActivation" playerIdentityId
      identityStep playerIdentityId MinionsActivate
      pure pid
    -- read once the villain has activated: a minion its boost card put into play
    -- activates too (Green Goblin insert, "Goblin Minions Activation Timing")
    MinionsActivate -> do
      let activate f =
            unless (null playerIdentityMinions) $
              chooseOneAtATimePrompted playerIdentityId "$label.chooseMinionToActivate" $
                [targeting minionId (f minionId) | minionId <- toList playerIdentityMinions]
      case currentIdentity pid of
        HeroSide _ -> activate \minionId -> enemyAttacks minionId playerIdentityId
        AlterEgoSide _ -> activate minionSchemes
      pure pid
    DealtEncounterCard ec ->
      pure $ PlayerIdentity $ attrs & encounterCardsL %~ (<> [ec])
    -- one at a time in the order dealt, a card dealt meanwhile (by surge) joining them
    RevealEncounterCards -> case playerIdentityEncounterCards of
      [] -> pure pid
      ec : rest -> do
        revealEncounterCard playerIdentityId ec
        identityStep playerIdentityId RevealEncounterCards
        pure $ PlayerIdentity $ attrs & encounterCardsL .~ rest
    IdentityWasAttacked attack' -> do
      -- Prevention changes damage taken. Piercing still resolves when the attack
      -- deals positive damage after defense, even when all of it is prevented.
      let piercingPrevented = attackPrevented attack' && attackPiercing attack'
            && subtractNatural playerIdentityDamageReduction (attackValue attack') > 0
          stripped = if piercingPrevented && playerIdentityTough then 1 + playerIdentityAdditionalTough else 0
      replicateM_ (fromIntegral stripped) $ checkWindows [W.Window W.After $ W.IdentityDiscardedTough playerIdentityId]
      senator <- selectOne (AllyWithTitle "Robert Kelly" <> AllyControlledBy (IdentityWithId playerIdentityId))
      let
        damage =
          subtractNatural playerIdentityDamageReduction $ attackDamageDealt attack'
      -- the damage, then retaliate (forced, so before any response; had as the attack
      -- resolves, RR Retaliate: Dauntless's condition is read then), then the window for
      -- responses to defending; a ranged attack ignores retaliate
      -- an attack dealing indirect damage (Starshark): the damage past the defense is
      -- divided among the characters the player controls, each share taken as it is
      when (damage > 0) $
        case attackThreatRemoval attack' of
          Just you -> removeThreat you (attackSource attack') damage MainScheme
          Nothing | Just enemy <- attackDamageRedirect attack' -> dealDamage enemy (attackSource attack') ((attackDamageTo attack' damage) {damageOverkill = attackOverkill attack'})
          Nothing | not (attackDefended attack'), Just aid <- senator -> damageCharacter aid (attackSource attack') (attackDamageTo attack' damage)
          Nothing -> if attackDividedEvenly attack'
            then divideAttackDamage playerIdentityId (attackSource attack') ((attackDamageTo attack' damage) {damageOverkill = attackOverkill attack'})
            else if attackIndirect attack'
            then dealIndirectAttackDamage playerIdentityId (attackSource attack') (attackDamageTo attack' damage)
            else damageCharacter playerIdentityId (attackSource attack') (attackDamageTo attack' damage)
      ignoresRetaliate <- elem AttacksIgnoreRetaliate <$> getModifiers (attackSource attack')
      unless (attackRanged attack' || ignoresRetaliate) $
        identityStep playerIdentityId (IdentityRetaliate $ attackEnemy attack')
      when (playerIdentityDefended || attackDefended attack') $
        identityStep playerIdentityId $ AttackDefended (attackEnemy attack') playerIdentityDamage playerIdentityDefended
      let final = attrs & damageReductionL .~ 0 & defendedL .~ False
      pure $ PlayerIdentity $ if stripped > 0 then final {playerIdentityTough = False, playerIdentityAdditionalTough = 0} else final
    IdentityDamaged source damage | damagePiercing damage && damageAmount damage > 0 -> do
      cardSpecific playerIdentityId "discardToughForPiercing" (source, damage)
      pure pid
    IdentityDamaged source damage -> do
      immune <- cannotTakeDamageFrom (IdentityRef playerIdentityId) source
      if immune then pure pid
      else if playerIdentityTough && damageAmount damage > 0 && preventedByTough damage
        then do
          checkWindows [W.Window W.After $ W.IdentityDiscardedTough playerIdentityId]
          pure $ PlayerIdentity $ removeOneIdentityTough attrs
        else do
          modifiedHp <- getModifiedHp pid
          -- what answers "would be defeated" may take the defeat off the queue
          attackDealtDamageTo source damage playerIdentityId
          when (playerIdentityDamage + damageAmount damage >= modifiedHp) $ defeatIdentityFromDamage playerIdentityId source damage
          when (damageAmount damage > 0) $
            checkWindows [W.Window W.After $ W.IdentityTookDamage playerIdentityId source damage]
          let fromAttack = if damageSource damage == FromAttack then damageAmount damage else 0
          pure $
            PlayerIdentity $
              attrs
                & damageL +~ damageAmount damage
                & toughL %~ (&& not (piercesTough damage))
                & \a -> a {playerIdentityAttackDamageTaken = playerIdentityAttackDamageTaken + fromAttack, playerIdentityAdditionalTough = if piercesTough damage then 0 else playerIdentityAdditionalTough}
    IdentityDefeated -> do
      players <- getAllPlayers
      let
        ps = take 1 . drop 1 $ dropWhile (/= playerIdentityId) (cycle players)
      -- the cards they control leave play, and the encounter cards dealt to them are
      -- discarded (rules reference, "Player Elimination"); their minions engage the
      -- next player; then they leave the game
      upgrades <- selectList (UpgradeControlledBy $ IdentityWithId playerIdentityId)
      -- an ally the first player controls goes with the first player token instead,
      -- passed first (RR Player Elimination, step 1: Odin)
      allies <- filterM (fmap (notElem ControlledByFirstPlayer) . getModifiers) (toList playerIdentityAllies)
      for_ allies removeFromPlay
      for_ (toList playerIdentitySupports) removeFromPlay
      for_ upgrades removeFromPlay
      for_ playerIdentityEncounterCards discardCard
      -- and the encounter cards in their play area (Ebony Maw's Spells)
      traverse_ removeFromPlay =<< selectList (EnvironmentInPlayAreaOf playerIdentityId)
      case ps of
        [x] | x /= playerIdentityId -> for_ (toList playerIdentityMinions) (`engageMinion` x)
        _ -> pure ()
      removeFromPlay playerIdentityId
      pure $ PlayerIdentity $ attrs & defeatedL .~ True & encounterCardsL .~ []
    AttackDefended enemy damageBefore basic -> do
      checkWindows $
          W.Window W.After (W.HeroDefends playerIdentityId enemy)
            : [W.Window W.After (W.MadeBasicDefense playerIdentityId enemy) | basic]
            <> [ W.Window W.After (W.DefendedWithoutDamage playerIdentityId enemy)
              | playerIdentityDamage <= damageBefore
              ]
      pure pid
    IdentityDefended n ->
      pure $ PlayerIdentity $ attrs & damageReductionL +~ n & defendedL .~ True
    PlaceCountersUpTo n cap -> do
      let placed = min n (subtractNatural playerIdentityCounters cap)
      when (placed > 0) $ placeCounters playerIdentityId placed
      pure pid
    TopOfDeckIntoCollection -> do
      for_ (take 1 $ unDeck playerIdentityDeck) placeInCollection
      pure pid
    IdentityHealed n ->
      pure $ PlayerIdentity $ attrs & damageL %~ subtractNatural n
    IdentitySetHitPoints n -> do
      modifiedHp <- getModifiedHp pid
      pure $ PlayerIdentity $ attrs & damageL .~ subtractNatural n modifiedHp
    IdentityStunned -> identityGivenStatus pid StunnedStatus
    IdentityBecomeTough -> identityGivenStatus pid ToughStatus
    IdentityConfused -> identityGivenStatus pid ConfusedStatus
    IdentityRemoveStunned -> pure $ PlayerIdentity $ (attrs & stunnedL .~ False) {playerIdentityAdditionalStun = False}
    IdentityRemoveConfused ->
      pure $ PlayerIdentity $ (attrs & confusedL .~ False) {playerIdentityAdditionalConfuse = False}
    Search (SearchIdentityDeck iid projection) cardMatcher searchOption returnOption
      | iid == playerIdentityId ->
          do
            let
              deck = unDeck playerIdentityDeck
              (focusedCards, deck') = case projection of
                AllOfDeck -> (deck, [])
                TopOfDeck n -> splitAt n deck
              (foundCards, rest) = partition (cardMatch cardMatcher) focusedCards
              handleReturnCards = case returnOption of
                ShuffleBackIn -> shuffleIntoDeck playerIdentityId
                DiscardRest -> traverse_ discardCard
                BottomInAnyOrder -> bottomInOrder
              -- the next card chosen goes under those placed before it
              bottomInOrder = \case
                [] -> pure ()
                [c] -> placeOnBottomOfDeck playerIdentityId c
                cs ->
                  chooseOnePrompted playerIdentityId "$label.chooseCardForBottomOfDeck"
                    [targeting c (placeOnBottomOfDeck playerIdentityId c >> bottomInOrder cs') | (c, cs') <- removeEach cs]
              handleFoundCards =
                if null foundCards
                  then
                    -- the cards looked at go back as the rest would
                    chooseOne playerIdentityId [labeled "$label.noMatchingCardsFound" (handleReturnCards rest)]
                  else case searchOption of
                    -- the card chosen goes where the target puts it, the rest back
                    SearchTarget target ->
                      chooseOne playerIdentityId
                        [ targeting c do
                            searchFoundCards target [PlayerCard c]
                            handleReturnCards (rest <> cs')
                        | (c, cs') <- removeEach foundCards
                        ]
                    SearchDrawOne ->
                      chooseOne playerIdentityId
                        [ targeting c do
                            addToHand playerIdentityId c
                            handleReturnCards (rest <> cs')
                        | (c, cs') <- removeEach foundCards
                        ]
            focusing (PlayerCard <$> focusedCards) handleFoundCards
            identityStep playerIdentityId FinishedSearching
            -- the cards looked at are out of the deck until the search is over: the deck
            -- is not empty meanwhile
            pure $ PlayerIdentity $ attrs & deckL .~ Deck deck' & \a -> a {playerIdentitySearching = True}
    -- the cards stay where they are while searched; the deck is shuffled after
    Search (SearchIdentityDeckAndDiscardPile iid) cardMatcher searchOption ShuffleBackIn
      | iid == playerIdentityId -> do
          let found = filter (cardMatch cardMatcher) (unDeck playerIdentityDeck <> unDiscard playerIdentityDiscard)
          focusing (PlayerCard <$> found) $
            chooseOne playerIdentityId $
              [labeled "$label.noMatchingCardsFound" (pure ()) | null found]
                <> [targeting c (case searchOption of SearchDrawOne -> addToHand playerIdentityId c; SearchTarget target -> searchFoundCards target [PlayerCard c]) | c <- found]
          shuffleDeck playerIdentityId
          pure pid
    FinishedSearching -> pure $ PlayerIdentity attrs {playerIdentitySearching = False}
    DiscardFromDeckUntil matcher target -> do
      let
        (missed, rest) = break (cardMatch matcher) (unDeck playerIdentityDeck)
        found = take 1 rest
      discarded <- discardPlayerDeckEncounters playerIdentityId (missed <> found)
      actor <- getYou
      for_ discarded $ \card -> checkWindows [W.Window W.After $ W.DiscardedFromPlayerDeck playerIdentityId [card], W.Window W.After $ W.PlayerDiscardedFromDeck actor playerIdentityId card]
      for_ [c | c <- found, isNothing (playerZoneEncounterCard c)] \c -> withDiscardedMatch target FromDeck (PlayerCard c)
      pure $
        PlayerIdentity $
          attrs
            & (discardL %~ Discard . (reverse discarded <>) . unDiscard)
            & (deckL .~ Deck (drop 1 rest))
    PlaceOnBottomOfDeck card ->
      pure $
        PlayerIdentity $
          attrs
            & (discardL %~ Discard . filter (/= card) . unDiscard)
            & (handL %~ Hand . filter (/= card) . unHand)
            & (deckL %~ Deck . (<> [card]) . filter (/= card) . unDeck)
    PlaceOnTopOfDeck card ->
      pure $
        PlayerIdentity $
          attrs
            & (discardL %~ Discard . filter (/= card) . unDiscard)
            & (handL %~ Hand . filter (/= card) . unHand)
            & (deckL %~ Deck . (card :) . filter (/= card) . unDeck)
    Search _ _ _ _ -> error "Unhandled"
    IdentityRetaliate enemyId -> do
      -- each instance of retaliate adds to the others
      n <- sum . map (\case Retaliate x -> x; _ -> 0) <$> getModifiedKeywords pid
      when (n > 0) $
        resolveAs playerIdentityId $ DamageEnemy (enemyRef enemyId) (toSource pid) (toDamage n FromRetaliate)
      pure pid
    SideMessage _ -> case currentIdentity pid of
      HeroSide x -> do
        newSide <-
          HeroSide <$> runMessage (IdentityMessage playerIdentityId msg) x
        pure $
          PlayerIdentity $
            attrs
              & sidesL
                . at playerIdentitySide
                ?~ newSide
      AlterEgoSide x -> do
        newSide <-
          AlterEgoSide <$> runMessage (IdentityMessage playerIdentityId msg) x
        pure $
          PlayerIdentity $
            attrs
              & sidesL
                . at playerIdentitySide
                ?~ newSide

-- | A player deck that empties is made again from its discard pile at once, and its
-- player dealt a facedown encounter card; with nothing in the discard pile, as soon as
-- there is.
instance RunMessage PlayerIdentity where
  runMessage msg pid = do
    pid' <- runQueueT $ runPlayerMessage msg pid
    let a = toAttrs pid'
    -- queued after what the message did, so the deck is made again before any of it runs
    runQueueT $ when (null (unDeck a.playerIdentityDeck) && notNull (unDiscard a.playerIdentityDiscard) && not a.playerIdentitySearching) do
      resetDeck a.playerIdentityId
    modifiers <- getModifiers pid'
    let keywords = toList (cdKeywords $ getCardDef pid') <> [k | KeywordModifier k <- modifiers]
        steady = Steady `elem` keywords
        continuousStun = ContinuouslyStunned `elem` modifiers && Stalwart `notElem` keywords
        continuousConfuse = ContinuouslyConfused `elem` modifiers && Stalwart `notElem` keywords
        updated = (toAttrs pid')
          { playerIdentityStunned = playerIdentityStunned a || continuousStun
          , playerIdentityConfused = playerIdentityConfused a || continuousConfuse
          , playerIdentityAdditionalStun = steady && (playerIdentityAdditionalStun a || continuousStun)
          , playerIdentityAdditionalConfuse = steady && (playerIdentityAdditionalConfuse a || continuousConfuse)
          }
    pure $ PlayerIdentity updated

runPlayerMessage ::
  (ReverseQueue m) =>
  Message ->
  PlayerIdentity ->
  m PlayerIdentity
runPlayerMessage msg pid@(PlayerIdentity attrs) = case msg of
    CardSpecific (IdentityRef ident) "swapHandWithDeckTop" (fromJSON -> Success card)
      | ident == playerIdentityId attrs, card `elem` unHand (playerIdentityHand attrs)
      , top : rest <- unDeck (playerIdentityDeck attrs) -> pure $ PlayerIdentity attrs
          {playerIdentityHand = Hand (map (\c -> if c == card then top else c) (unHand $ playerIdentityHand attrs)), playerIdentityDeck = Deck (card : rest)}
    CardSpecific (IdentityRef ident) "swapHandWithDiscardTop" (fromJSON -> Success card)
      | ident == playerIdentityId attrs, card `elem` unHand (playerIdentityHand attrs)
      , top : rest <- unDiscard (playerIdentityDiscard attrs) -> pure $ PlayerIdentity attrs
          {playerIdentityHand = Hand (map (\c -> if c == card then top else c) (unHand $ playerIdentityHand attrs)), playerIdentityDiscard = Discard (card : rest)}
    CardSpecific (IdentityRef ident) "readyWithCostPaid" _ | ident == playerIdentityId attrs -> do
      checkWindows [W.Window W.After $ W.IdentityReadied ident]
      pure $ PlayerIdentity attrs {playerIdentityExhausted = False}
    CardSpecific (IdentityRef ident) "discardToughForPiercing" (fromJSON -> Success (source :: Source, damage :: Damage)) | ident == playerIdentityId attrs -> do
      immune <- cannotTakeDamageFrom (IdentityRef ident) source
      let count = if playerIdentityTough attrs && not immune then 1 + playerIdentityAdditionalTough attrs else 0
      replicateM_ (fromIntegral count) $ checkWindows [W.Window W.After $ W.IdentityDiscardedTough ident]
      -- Piercing is processed once. A new tough granted by a response can prevent the damage.
      damageIdentity ident source damage {damagePiercing = False}
      pure $ if count == 0 then pid else PlayerIdentity attrs {playerIdentityTough = False, playerIdentityAdditionalTough = 0}
    CardSpecific target "playChosenX" (fromJSON -> Success (card, mWindow :: Maybe W.Window, mTarget, n :: Natural))
      | target == toRef pid -> do
          costly <- (> 0) <$> getModifiedCostWithBase pid card mTarget (Just $ fromIntegral n)
          when (card `elem` unHand (playerIdentityHand attrs) && costly) $
            checkWindows [W.Window W.When $ W.PlayingCardFromHand (toId pid) card]
          cardSpecific pid "payForChosenX" (card, mWindow, mTarget, n)
          pure pid
    CardSpecific target "payForChosenX" (fromJSON -> Success (card, mWindow :: Maybe W.Window, mTarget, n :: Natural))
      | target == toRef pid -> beginCardPayment (Just n) pid card mWindow mTarget
    DiscardedCard (PlayerCard card)
      | Just encounter <- playerZoneEncounterCard card
      , pcController card == Just (playerIdentityId attrs) -> do
          discardCard encounter
          pure $ PlayerIdentity $ attrs
            & (discardL %~ Discard . filter (/= card) . unDiscard)
            & (handL %~ Hand . filter (/= card) . unHand)
            & (deckL %~ Deck . filter (/= card) . unDeck)
    DiscardedCard (PlayerCard card)
      | pcOwner card == Just (playerIdentityId attrs) ->
          pure $
            PlayerIdentity $
              attrs
                & (discardL %~ Discard . (card :) . unDiscard)
                & (handL %~ Hand . filter (/= card) . unHand)
    -- into The Collection from the player's hand, deck or discard pile
    PlaceInCollection (PlayerCard card) ->
      pure $
        PlayerIdentity $
          attrs
            & (discardL %~ Discard . filter (/= card) . unDiscard)
            & (handL %~ Hand . filter (/= card) . unHand)
            & (deckL %~ Deck . filter (/= card) . unDeck)
    RemoveFromGame (CardIdRef cid) ->
      pure $
        PlayerIdentity $
          attrs
            & (discardL %~ Discard . filter ((/= cid) . pcCardId) . unDiscard)
            & (handL %~ Hand . filter ((/= cid) . pcCardId) . unHand)
            & (deckL %~ Deck . filter ((/= cid) . pcCardId) . unDeck)
    -- an upgrade's "gets +X hit points" ceasing reduces the hit point dial, which at
    -- zero defeats them
    UpgradeRemoved upgradeId -> do
      let ident = playerIdentityId attrs
          damage = playerIdentityDamage attrs
      when (HashSet.member upgradeId (playerIdentityUpgrades attrs) && not (playerIdentityDefeated attrs) && damage > 0) do
        modifiedHp <- getModifiedHp pid
        pending <- isQueued (== IdentityMessage ident IdentityDefeated)
        when (damage >= modifiedHp && not pending) $
          defeatIdentity ident
      -- its restricted limit may have dropped (Side Holster)
      when (HashSet.member upgradeId (playerIdentityUpgrades attrs) && not (playerIdentityDefeated attrs)) $
        identityStep ident CheckRestrictedLimit
      pure $ PlayerIdentity $ attrs & upgradesL %~ HashSet.delete upgradeId
    PlaceCounters (IdentityRef ident) n | ident == playerIdentityId attrs ->
      pure $ PlayerIdentity attrs {playerIdentityCounters = playerIdentityCounters attrs + n}
    RemoveCounters (IdentityRef ident) n | ident == playerIdentityId attrs ->
      pure $ PlayerIdentity attrs {playerIdentityCounters = subtractNatural n (playerIdentityCounters attrs)}
    GiveStatus (IdentityRef ident) s | ident == playerIdentityId attrs -> identityGivenStatus pid s
    RemoveStatus (IdentityRef ident) ToughStatus | ident == playerIdentityId attrs -> do
      when (playerIdentityTough attrs) $ checkWindows [W.Window W.After $ W.IdentityDiscardedTough ident]
      pure $ PlayerIdentity $ removeOneIdentityTough attrs
    RemoveStatus (IdentityRef ident) StunnedStatus | ident == playerIdentityId attrs, playerIdentityAdditionalStun attrs -> pure $ PlayerIdentity attrs {playerIdentityAdditionalStun = False}
    RemoveStatus (IdentityRef ident) ConfusedStatus | ident == playerIdentityId attrs, playerIdentityAdditionalConfuse attrs -> pure $ PlayerIdentity attrs {playerIdentityAdditionalConfuse = False}
    RemoveStatus (IdentityRef ident) s | ident == playerIdentityId attrs ->
      pure $ PlayerIdentity $ attrs & statusL s .~ False
    -- an enemy's attack begins: none of its damage taken yet, by the player attacked or
    -- by one whose hero defends it
    VillainMessage _ (VillainBeginAttack _) -> attackBegins
    MinionMessage _ (MinionBeginAttack _) -> attackBegins
    -- Setup abilities run after the opening hand and mulligan. A card found in
    -- any of the player's zones is removed from that zone without being discarded.
    SetAside cards -> do
      let ids = [pcCardId c | PlayerCard c <- cards]
          keep c = pcCardId c `notElem` ids
      pure $ PlayerIdentity $ attrs
        & (handL %~ Hand . filter keep . unHand)
        & (discardL %~ Discard . filter keep . unDiscard)
        & (deckL %~ Deck . filter keep . unDeck)
    -- a card put into play leaves wherever it was
    PutCardIntoPlay _ card _ _ _ ->
      pure $
        PlayerIdentity $
          attrs
            & (handL %~ Hand . filter (/= card) . unHand)
            & (discardL %~ Discard . filter (/= card) . unDiscard)
            & (deckL %~ Deck . filter (/= card) . unDeck)
    IdentityMessage ident msg'
      | ident == playerIdentityId attrs ->
          runIdentityMessage msg' pid
    -- a step for the identity card is for it whichever face is up
    CardSpecific (IdentityRef ident) _ _ | ident == playerIdentityId attrs -> do
      sides <- for (playerIdentitySides attrs) \case
        HeroSide x -> HeroSide <$> runMessage msg x
        AlterEgoSide x -> AlterEgoSide <$> runMessage msg x
      pure $ PlayerIdentity attrs {playerIdentitySides = sides}
    _ -> passToSide
 where
  attackBegins = do
    pid' <- passToSide
    pure $ PlayerIdentity (toAttrs pid') {playerIdentityAttackDamageTaken = 0}
  passToSide = case currentIdentity pid of
      HeroSide x -> do
        newSide <- HeroSide <$> runMessage msg x
        pure $
          PlayerIdentity $
            attrs
              & sidesL
                . at (playerIdentitySide attrs)
                ?~ newSide
      AlterEgoSide x -> do
        newSide <- AlterEgoSide <$> runMessage msg x
        pure $
          PlayerIdentity $
            attrs
              & sidesL
                . at (playerIdentitySide attrs)
                ?~ newSide

instance HasStartingHP PlayerIdentity where
  startingHP a = case currentIdentity a of
    HeroSide x -> startingHP x
    AlterEgoSide x -> startingHP x

instance HasHandSize PlayerIdentity where
  handSize a = case currentIdentity a of
    HeroSide x -> handSize x
    AlterEgoSide x -> handSize x

instance HasCardCode PlayerIdentity where
  toCardCode a = case currentIdentity a of
    HeroSide x -> toCardCode x
    AlterEgoSide x -> toCardCode x

instance HasCardDef PlayerIdentity where
  getCardDef a = case currentIdentity a of
    HeroSide x -> getCardDef x
    AlterEgoSide x -> getCardDef x

instance HasModifiersFor PlayerIdentity where
  getModifiersFor source target a = case currentIdentity a of
    HeroSide x -> getModifiersFor source target x
    AlterEgoSide x -> getModifiersFor source target x

-- | How many restricted upgrades the player can control: any, and besides those, Weapons
-- only.
getModifiedRestrictedLimits :: (HasGame m) => PlayerIdentity -> m (Natural, Natural)
getModifiedRestrictedLimits pid = do
  modifiers <- getModifiers pid
  pure (2 + sum [n | RestrictedLimitModifier n <- modifiers], sum [n | RestrictedWeaponLimitModifier n <- modifiers])

getModifiedAllyLimit :: (HasGame m) => PlayerIdentity -> m Natural
getModifiedAllyLimit pid@(PlayerIdentity attrs) = do
  modifiers <- getModifiers pid
  pure $ foldr applyModifier (playerIdentityAllyLimit attrs) modifiers
 where
  applyModifier (AllyLimitModifier n) = max 0 . (+ fromIntegral n)
  applyModifier (AllyLimitReduction n) = subtractNatural n
  applyModifier _ = id

deriving anyclass instance FromJSON PlayerIdentitySide

instance ToJSON (Attrs PlayerIdentity) where
  toJSON = genericToJSON $ aesonOptions $ Just "playerIdentity"

instance FromJSON (Attrs PlayerIdentity) where
  parseJSON = withObject "PlayerIdentityAttrs" $ \o ->
    genericParseJSON (aesonOptions $ Just "playerIdentity") $ Object $
      KeyMap.union o $ KeyMap.fromList
        [("weatherDeck", Null), ("additionalTough", toJSON (0 :: Natural)), ("boostCards", toJSON ([] :: [EncounterCard])), ("additionalStun", Bool False), ("additionalConfuse", Bool False)]

deriving newtype instance ToJSON PlayerIdentity
deriving newtype instance FromJSON PlayerIdentity


-- | The cards of a hand a player may choose to discard: not System Shock ("You cannot
-- choose to discard this card from your hand"); a random discard still takes it.
choosableToDiscard :: [PlayerCard] -> [PlayerCard]
choosableToDiscard = filter (not . staysInHand)
