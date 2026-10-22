{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Identity (
  module Marvel.Identity,
  module Marvel.Identity.Types,
) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (find, partition)
import Marvel.Ability
import Marvel.ActiveCost
import Marvel.AlterEgo
import Marvel.AlterEgo.Types
import Marvel.Attack
import Marvel.Card
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

identityIsStunned :: PlayerIdentity -> Bool
identityIsStunned = playerIdentityStunned . toAttrs

identityIsConfused :: PlayerIdentity -> Bool
identityIsConfused = playerIdentityConfused . toAttrs

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
    modifiers <- getModifiers pIdentity
    traits <- case currentIdentity pIdentity of
      HeroSide x -> getTraits x
      AlterEgoSide x -> getTraits x
    pure $ foldr applyModifier traits modifiers
   where
    applyModifier (TraitModifier t) = HashSet.insert t
    applyModifier _ = id

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
      , playerIdentityDamageReduction = 0
      , playerIdentityStunned = False
      , playerIdentityConfused = False
      , playerIdentityTough = False
      , playerIdentityDefeated = False
      , playerIdentityDefended = False
      , playerIdentityAttackDamageTaken = 0
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
isPlayable attrs c = do
  affordable <- notNull <$> playableTargets attrs c
  passedCriteria <- passesCardCriteria ident c (cdCriteria def <> toAdditionalCriteria def)
  passedPlayLimits <- checkPlayLimits attrs c
  matchesUnique <-
    if cdUnique def
      then any (uniqueMatch (title $ cdName def, maybe [] pure (subtitle $ cdName def))) <$> getUniqueTitlesInPlay
      else pure False
  pure $
    affordable
      && passedCriteria
      && passedPlayLimits
      && not matchesUnique
      && isNothing (cdResponseWindow def)
      && (cdCardType def /= ResourceType)
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

checkPlayLimits :: (HasGame m) => PlayerIdentity -> PlayerCard -> m Bool
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
    MaxPerAlly 1 -> case cdCardType def of
      UpgradeType -> selectAny $ NotAlly $ AllyWithUpgrade $ UpgradeNamed name
      _ -> error "Unhandled"
    MaxPerAlly _ -> error "Unhandled"
    MaxPerEnemy 1 -> case cdCardType def of
      UpgradeType -> selectAny $ NotEnemy $ EnemyWithUpgrade $ UpgradeNamed name
      _ -> error "Unhandled"
    MaxPerEnemy _ -> error "Unhandled"
    -- every such card attaches to the main scheme, of which there is one: one in play
    -- is one on it
    MaxPerScheme 1 -> selectNone $ UpgradeNamed name
    MaxPerScheme _ -> error "Unhandled"
    -- copies played by any player count
    MaxPerRound n -> (< n) . count ((== name) . cdName . snd) <$> getPlayedThisRound
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

-- | The resource cost of playing the card on the target, as modified: Iron Man's "reduce
-- the cost to play each upgrade on Iron Man" answers for the character it is played on.
getModifiedCost :: (HasGame m) => PlayerIdentity -> PlayerCard -> Maybe Target -> m Int
getModifiedCost attrs c mTarget = do
  modifiers <- (<>) <$> getModifiers attrs <*> getModifiers c
  onTarget <- maybe (pure []) getModifiers mTarget
  pure $
    maybe 0 (max 0 . (+ sum (map change modifiers <> map changeOn onTarget))) (cdCost $ getCardDef c)
 where
  change = \case
    ResourceCostReduction n -> negate (fromIntegral n)
    ResourceCostReductionFor matcher n | cardMatch matcher c -> negate (fromIntegral n)
    AdditionalResourceCost matcher n | cardMatch matcher c -> fromIntegral n
    _ -> 0
  changeOn = \case
    UpgradeOnThisCostReduction n | cdCardType (getCardDef c) == UpgradeType -> negate (fromIntegral n)
    _ -> 0

-- | The whole cost of playing the card on the target: its resource cost, and the cost
-- of its ability (an event's "→"), paid together.
getCardCost ::
  (HasQueue m, MonadThrow m, Projection m PlayerIdentity, HasGame m) =>
  PlayerIdentity ->
  PlayerCard ->
  Maybe Target ->
  m Cost
getCardCost attrs c mTarget = do
  modifiedCost <- getModifiedCost attrs c mTarget
  printed <- case (cdAbilityCost def, mTarget) of
    (abilityCost, Just (CardIdRef cid)) | usesTargetCost abilityCost -> do
      cards <- concatMapM (projectMap PlayerIdentityDiscard unDiscard) =<< getPlayers
      pure $ maybe 0 (fromMaybe 0 . cdCost . getCardDef) (find ((== cid) . pcCardId) cards)
    _ -> pure 0
  pure $
    mconcat (replicate modifiedCost (ResourceCost $ spendOnly $ cdCriteria def))
      <> mconcat (replicate printed (ResourceCost Nothing))
      <> filterCosts (/= TargetPrintedCost) (cdAbilityCost def)
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
playableTargets attrs c = do
  targets <- maybe [Nothing] (map Just) <$> getPlayTargets (toId attrs) c
  flip filterM targets \mTarget -> do
    cost <- getCardCost attrs c mTarget
    resources <- filter (canSpendOn $ cdCriteria $ getCardDef c) <$> getAvailableResourcesFor (toId attrs) (Just c)
    (length resources >= length (costResources cost) &&)
      <$> canAffordCost (toId attrs) (toSource c) (otherCosts cost)

getPlayableCards :: (HasQueue m, MonadRandom m, MonadThrow m, Projection m PlayerIdentity, HasGame m) => PlayerIdentity -> m [PlayerCard]
getPlayableCards player = do
  held <- getUpgradeHeldCards (toId player)
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
  usedAbilities <- getUsedAbilities
  playableCards <- getPlayableCards attrs
  validAbilities <-
    filterM
      ( andM
          . sequence
            [ pure . passesUseLimit ident usedAbilities
            , passesCriteria ident
            , passesCanAffordCost ident
            , pure . passesTiming ident
            , passesTypeIsRelevant ident
            ]
      )
      abilities
  pure $
    map UseAbility validAbilities
      <> map (($ Nothing) . PlayCard) playableCards

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

runIdentityMessage ::
  (ReverseQueue m) =>
  IdentityMessage ->
  PlayerIdentity ->
  m PlayerIdentity
runIdentityMessage msg pid@(PlayerIdentity attrs@PlayerIdentityAttrs {..}) =
  case msg of
    SetupIdentity -> do
      let nemesisSet = getNemesisSet $ toCardCode pid
      obligations <- getObligations pid
      nemesisSetCards <- gatherEncounterSet nemesisSet
      setAside (EncounterCard <$> nemesisSetCards)
      shuffleIntoEncounterDeck obligations
      shuffleDeck playerIdentityId
      pure pid
    -- the setup keyword: "begins the game in play"
    PutSetupCardsIntoPlay -> do
      for_ (filter (elem Keyword.Setup . cdKeywords . getCardDef) $ unDeck playerIdentityDeck) (putIntoPlay playerIdentityId)
      pure pid
    -- draw up to hand size, mulligan any of it, draw again, then the setup abilities
    DealStartingHand -> do
      let setupAbilities = filter ((== Setup) . abilityType) $ getAbilities pid
      for_ [DrawOrDiscardToHandLimit, DiscardCards, DrawOrDiscardToHandLimit] (identityStep playerIdentityId)
      for_ setupAbilities \a -> ranAbility (toId pid) (toTarget pid) (abilityIndex a) [] NoPayment
      pure pid
    BeginTurn -> do
      -- before anything else is done in the turn
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
          fromIntegral modifiedHandSize - length (unHand playerIdentityHand)
      when (diff > 0) $ drawCards playerIdentityId (fromIntegral diff)
      when (diff < 0) $
        chooseOnePrompted
          playerIdentityId
          "$label.discardToHandSize"
          [ targeting c do
            discardCard c
            identityStep playerIdentityId DrawOrDiscardToHandLimit
          | c <- unHand playerIdentityHand
          ]
      pure pid
    DrawToHandLimit -> do
      modifiedHandSize <- getModifiedHandSize pid
      let
        diff =
          fromIntegral modifiedHandSize - length (unHand playerIdentityHand)
      when (diff > 0) $ drawCards playerIdentityId (fromIntegral diff)
      pure pid
    DrawToPrintedHandSize -> do
      let short = fromIntegral (unHandSize $ handSize pid) - length (unHand playerIdentityHand)
      when (short > 0) $ drawCards playerIdentityId (fromIntegral short)
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
        let (obligations, drawn) = partition ((== ObligationType) . cdCardType . getCardDef) cards
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
      for_ (HashSet.toList playerIdentityAllies) ready
      for_ (HashSet.toList playerIdentitySupports) ready
      for_ (HashSet.toList playerIdentityUpgrades) ready
      readies <- notElem CannotReady <$> getModifiers pid
      pure $ PlayerIdentity $ attrs & exhaustedL %~ (&& not readies)
    -- an identity that cannot change form stays as it is, whatever would change it
    ChooseOtherForm -> do
      canChange <- identityMatches IdentityCanChangeForm playerIdentityId
      let
        otherForms = filter (/= playerIdentitySide) $ HashMap.keys playerIdentitySides
      when canChange $ chooseOrRunOne playerIdentityId $ map (pure . ChangeToForm) otherForms
      pure pid
    ChangedToForm side -> do
      canChange <- identityMatches IdentityCanChangeForm playerIdentityId
      if canChange
        then do
          checkWindows [W.Window W.After $ W.IdentityChangesForm playerIdentityId]
          pure $ PlayerIdentity $ attrs & sideL .~ side
        else pure pid
    PlayingCard card mWindow -> do
      targets <- playableTargets pid card
      let payFor = identityStep playerIdentityId . PayForCard card mWindow
      case targets of
        [mTarget] -> payFor mTarget
        _ -> chooseOnePromptedAbout playerIdentityId "$label.playOn" card [targeting t (payFor (Just t)) | Just t <- targets]
      pure pid
    -- a card played from a discard pile is left there until it is played, so that
    -- giving it up leaves it where it was
    PayForCard card mWindow mTarget -> do
      cost <- getCardCost pid card mTarget
      activeCostId <- getRandom
      setActiveCost $
          ActiveCost
            activeCostId
            playerIdentityId
            (ForCard card)
            cost
            (maybe NoPayment TargetPayment mTarget)
            mWindow
            mempty
      pure $ PlayerIdentity $ attrs & (handL %~ Hand . filter (/= card) . unHand)
    PlayedCard {} -> pure pid
    PaidWithCard card -> do
      mActiveCost <- getActiveCost
      case mActiveCost of
        Nothing -> error "no cost"
        Just activeCostId -> do
          spendCard activeCostId card
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
      when (HashSet.size playerIdentityAllies > limit) $
        chooseOnePrompted
          playerIdentityId
          "$label.discardAnAlly"
          [targeting aid (removeFromPlay aid) | aid <- toList playerIdentityAllies]
      pure pid
    UpgradeCreated upgradeId -> do
      identityStep playerIdentityId CheckRestrictedLimit
      pure $ PlayerIdentity $ attrs & upgradesL %~ HashSet.insert upgradeId
    CheckRestrictedLimit -> do
      restricted <- selectList $ UpgradeWithKeyword Restricted <> upgradeControlledBy playerIdentityId
      when (length restricted > 2) $
        chooseOnePrompted
          playerIdentityId
          "$label.discardARestrictedCard"
          [targeting u (removeFromPlay u) | u <- restricted]
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
    AddToHand card ->
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
                 | c <- unHand playerIdentityHand
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
              | c <- unHand playerIdentityHand
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
        pure $
          PlayerIdentity $
            attrs
              & (discardL %~ Discard . (discards <>) . unDiscard)
              & (handL %~ Hand . filter (`notElem` discards) . unHand)
      FromDeck -> do
        let
          (cards, deck') = splitAt (fromIntegral n) $ unDeck playerIdentityDeck
        for_ mTarget \target -> withDiscarded target fromZone $ PlayerCard <$> cards
        pure $
          PlayerIdentity $
            attrs
              & (discardL %~ Discard . (cards <>) . unDiscard)
              & (deckL .~ Deck deck')
    ReadiedIdentity -> do
      readies <- notElem CannotReady <$> getModifiers pid
      pure $ PlayerIdentity $ attrs & exhaustedL %~ (&& not readies)
    ExhaustedIdentity -> pure $ PlayerIdentity $ attrs & exhaustedL .~ True
    -- the active villain of the player's area, if it has one, then their minions
    VillainAndMinionsActivate -> do
      villain <- selectOne ActiveVillain
      case currentIdentity pid of
        HeroSide _ -> for_ villain (`enemyAttacks` playerIdentityId)
        AlterEgoSide _ -> for_ villain villainSchemes
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
      keywords <- getModifiedKeywords pid
      let
        damage =
          subtractNatural playerIdentityDamageReduction $ attackDamageDealt attack'
        -- each instance of retaliate adds to the others; a ranged attack ignores it
        retaliate = if attackRanged attack' then 0 else sum [n | Retaliate n <- keywords]
      -- the damage, then retaliate (forced, so before any response), then the window
      -- for responses to defending
      when (damage > 0) do
        checkWindows [W.Window W.When $ W.IdentityTakeDamage playerIdentityId (attackDamageTo attack' damage)]
        attackDamageIdentity playerIdentityId (attackSource attack') (attackDamageTo attack' damage)
      when (retaliate > 0) $
        identityStep playerIdentityId (IdentityRetaliate retaliate $ attackEnemy attack')
      when playerIdentityDefended $
        identityStep playerIdentityId $ AttackDefended (attackEnemy attack') playerIdentityDamage
      pure $ PlayerIdentity $ attrs & damageReductionL .~ 0 & defendedL .~ False
    -- a tough status card prevents all of it, and is discarded instead
    IdentityDamaged _ damage
      | playerIdentityTough && damageAmount damage > 0 && preventedByTough damage ->
          pure $ PlayerIdentity $ attrs & toughL .~ False
    IdentityDamaged source damage -> do
      modifiedHp <- getModifiedHp pid
      -- what answers "would be defeated" may take the defeat off the queue
      attackDealtDamageTo source damage playerIdentityId
      when (playerIdentityDamage + damageAmount damage >= modifiedHp) $ defeatIdentity playerIdentityId
      when (damageAmount damage > 0) $
        checkWindows [W.Window W.After $ W.IdentityTookDamage playerIdentityId source damage]
      let fromAttack = if damageSource damage == FromAttack then damageAmount damage else 0
      pure $
        PlayerIdentity $
          attrs
            & damageL +~ damageAmount damage
            & toughL %~ (&& not (piercesTough damage))
            & \a -> a {playerIdentityAttackDamageTaken = playerIdentityAttackDamageTaken + fromAttack}
    IdentityDefeated -> do
      players <- getPlayers
      let
        ps = take 1 . drop 1 $ dropWhile (/= playerIdentityId) (cycle players)
      -- the cards they control leave play, and the encounter cards dealt to them are
      -- discarded (rules reference, "Player Elimination"); their minions engage the
      -- next player; then they leave the game
      upgrades <- selectList (UpgradeControlledBy $ IdentityWithId playerIdentityId)
      for_ (toList playerIdentityAllies) removeFromPlay
      for_ (toList playerIdentitySupports) removeFromPlay
      for_ upgrades removeFromPlay
      for_ playerIdentityEncounterCards discardCard
      case ps of
        [x] | x /= playerIdentityId -> for_ (toList playerIdentityMinions) (`engageMinion` x)
        _ -> pure ()
      removeFromPlay playerIdentityId
      pure $ PlayerIdentity $ attrs & defeatedL .~ True & encounterCardsL .~ []
    AttackDefended enemy damageBefore -> do
      checkWindows $
          W.Window W.After (W.HeroDefends playerIdentityId enemy)
            : [ W.Window W.After (W.DefendedWithoutDamage playerIdentityId enemy)
              | playerIdentityDamage <= damageBefore
              ]
      pure pid
    IdentityDefended n ->
      pure $ PlayerIdentity $ attrs & damageReductionL +~ n & defendedL .~ True
    IdentityHealed n ->
      pure $ PlayerIdentity $ attrs & damageL %~ subtractNatural n
    IdentitySetHitPoints n -> do
      modifiedHp <- getModifiedHp pid
      pure $ PlayerIdentity $ attrs & damageL .~ subtractNatural n modifiedHp
    IdentityStunned -> pure $ PlayerIdentity $ attrs & stunnedL .~ True
    IdentityBecomeTough -> pure $ PlayerIdentity $ attrs & toughL .~ True
    IdentityConfused -> pure $ PlayerIdentity $ attrs & confusedL .~ True
    IdentityRemoveStunned -> pure $ PlayerIdentity $ attrs & stunnedL .~ False
    IdentityRemoveConfused ->
      pure $ PlayerIdentity $ attrs & confusedL .~ False
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
    Search (SearchIdentityDeckAndDiscardPile iid) cardMatcher SearchDrawOne ShuffleBackIn
      | iid == playerIdentityId -> do
          let found = filter (cardMatch cardMatcher) (unDeck playerIdentityDeck <> unDiscard playerIdentityDiscard)
          focusing (PlayerCard <$> found) $
            chooseOne playerIdentityId $
              [labeled "$label.noMatchingCardsFound" (pure ()) | null found]
                <> [targeting c (addToHand playerIdentityId c) | c <- found]
          shuffleDeck playerIdentityId
          pure pid
    FinishedSearching -> pure $ PlayerIdentity attrs {playerIdentitySearching = False}
    DiscardFromDeckUntil matcher target -> do
      let
        (missed, rest) = break (cardMatch matcher) (unDeck playerIdentityDeck)
        found = take 1 rest
      for_ found \c -> withDiscardedMatch target FromDeck (PlayerCard c)
      pure $
        PlayerIdentity $
          attrs
            & (discardL %~ Discard . (reverse (missed <> found) <>) . unDiscard)
            & (deckL .~ Deck (drop 1 rest))
    PlaceOnBottomOfDeck card ->
      pure $
        PlayerIdentity $
          attrs
            & (discardL %~ Discard . filter (/= card) . unDiscard)
            & (handL %~ Hand . filter (/= card) . unHand)
            & (deckL %~ Deck . (<> [card]) . filter (/= card) . unDeck)
    Search _ _ _ _ -> error "Unhandled"
    IdentityRetaliate n enemyId -> do
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
      identityStep a.playerIdentityId ShuffleIdentityDiscardBackIntoDeck
      dealEncounterCard a.playerIdentityId
    pure pid'

runPlayerMessage ::
  (ReverseQueue m) =>
  Message ->
  PlayerIdentity ->
  m PlayerIdentity
runPlayerMessage msg pid@(PlayerIdentity attrs) = case msg of
    DiscardedCard (PlayerCard card)
      | pcOwner card == Just (playerIdentityId attrs) ->
          pure $
            PlayerIdentity $
              attrs
                & (discardL %~ Discard . (card :) . unDiscard)
                & (handL %~ Hand . filter (/= card) . unHand)
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
        when (damage >= modifiedHp) $
          defeatIdentity ident
      pure $ PlayerIdentity $ attrs & upgradesL %~ HashSet.delete upgradeId
    GiveStatus (IdentityRef ident) s | ident == playerIdentityId attrs ->
      pure $ PlayerIdentity $ attrs & statusL s .~ True
    RemoveStatus (IdentityRef ident) s | ident == playerIdentityId attrs ->
      pure $ PlayerIdentity $ attrs & statusL s .~ False
    -- an enemy's attack begins: none of its damage taken yet, by the player attacked or
    -- by one whose hero defends it
    VillainMessage _ (VillainBeginAttack _) -> attackBegins
    MinionMessage _ (MinionBeginAttack _) -> attackBegins
    -- a card put into play leaves wherever it was
    PutCardIntoPlay _ card _ _ ->
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

getModifiedAllyLimit :: (HasGame m) => PlayerIdentity -> m Natural
getModifiedAllyLimit pid@(PlayerIdentity attrs) = do
  modifiers <- getModifiers pid
  pure $ foldr applyModifier (playerIdentityAllyLimit attrs) modifiers
 where
  applyModifier (AllyLimitModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

deriving anyclass instance FromJSON PlayerIdentitySide

instance ToJSON (Attrs PlayerIdentity) where
  toJSON = genericToJSON $ aesonOptions $ Just "playerIdentity"

instance FromJSON (Attrs PlayerIdentity) where
  parseJSON = genericParseJSON $ aesonOptions $ Just "playerIdentity"

deriving newtype instance ToJSON PlayerIdentity
deriving newtype instance FromJSON PlayerIdentity

