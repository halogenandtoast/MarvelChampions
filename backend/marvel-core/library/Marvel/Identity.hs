{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Identity
  ( module Marvel.Identity
  , module Marvel.Identity.Types
  ) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Data.List (partition)
import Marvel.Ability
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
import Marvel.Hero.Types
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Matchers qualified as Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target
import Marvel.Trait
import Marvel.Window qualified as W
import System.Random.Shuffle

passedL :: Lens' (Attrs PlayerIdentity) Bool
passedL = lens playerIdentityPassed $ \m x -> m { playerIdentityPassed = x }

exhaustedL :: Lens' (Attrs PlayerIdentity) Bool
exhaustedL =
  lens playerIdentityExhausted $ \m x -> m { playerIdentityExhausted = x }

encounterCardsL :: Lens' (Attrs PlayerIdentity) [EncounterCard]
encounterCardsL = lens playerIdentityEncounterCards
  $ \m x -> m { playerIdentityEncounterCards = x }

damageL :: Lens' (Attrs PlayerIdentity) Natural
damageL = lens playerIdentityDamage $ \m x -> m { playerIdentityDamage = x }

sideL :: Lens' (Attrs PlayerIdentity) Side
sideL = lens playerIdentitySide $ \m x -> m { playerIdentitySide = x }

sidesL :: Lens' (Attrs PlayerIdentity) (HashMap Side PlayerIdentitySide)
sidesL = lens playerIdentitySides $ \m x -> m { playerIdentitySides = x }

alliesL :: Lens' (Attrs PlayerIdentity) (HashSet AllyId)
alliesL = lens playerIdentityAllies $ \m x -> m { playerIdentityAllies = x }

minionsL :: Lens' (Attrs PlayerIdentity) (HashSet MinionId)
minionsL = lens playerIdentityMinions $ \m x -> m { playerIdentityMinions = x }

upgradesL :: Lens' (Attrs PlayerIdentity) (HashSet UpgradeId)
upgradesL =
  lens playerIdentityUpgrades $ \m x -> m { playerIdentityUpgrades = x }

supportsL :: Lens' (Attrs PlayerIdentity) (HashSet SupportId)
supportsL =
  lens playerIdentitySupports $ \m x -> m { playerIdentitySupports = x }

deckL :: Lens' (Attrs PlayerIdentity) Deck
deckL = lens playerIdentityDeck $ \m x -> m { playerIdentityDeck = x }

handL :: Lens' (Attrs PlayerIdentity) Hand
handL = lens playerIdentityHand $ \m x -> m { playerIdentityHand = x }

discardL :: Lens' (Attrs PlayerIdentity) Discard
discardL = lens playerIdentityDiscard $ \m x -> m { playerIdentityDiscard = x }

stunnedL :: Lens' (Attrs PlayerIdentity) Bool
stunnedL = lens playerIdentityStunned $ \m x -> m { playerIdentityStunned = x }

confusedL :: Lens' (Attrs PlayerIdentity) Bool
confusedL =
  lens playerIdentityConfused $ \m x -> m { playerIdentityConfused = x }

toughL :: Lens' (Attrs PlayerIdentity) Bool
toughL = lens playerIdentityTough $ \m x -> m { playerIdentityTough = x }

defeatedL :: Lens' (Attrs PlayerIdentity) Bool
defeatedL =
  lens playerIdentityDefeated $ \m x -> m { playerIdentityDefeated = x }

defendedL :: Lens' (Attrs PlayerIdentity) Bool
defendedL =
  lens playerIdentityDefended $ \m x -> m { playerIdentityDefended = x }

damageReductionL :: Lens' (Attrs PlayerIdentity) Natural
damageReductionL = lens playerIdentityDamageReduction
  $ \m x -> m { playerIdentityDamageReduction = x }

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

getModifiedHandSize :: MonadGame env m => PlayerIdentity -> m Natural
getModifiedHandSize pid = do
  modifiers <- getModifiers pid
  pure $ foldr applyModifier (unHandSize $ handSize pid) modifiers
 where
  applyModifier (HandSizeModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedHp :: MonadGame env m => PlayerIdentity -> m Natural
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

getIdentityHeroAttackDamage :: MonadGame env m => PlayerIdentity -> m Natural
getIdentityHeroAttackDamage attrs = case currentIdentity attrs of
  AlterEgoSide _ -> pure 0
  HeroSide x -> getModifiedAttack $ toAttrs x

instance Exhaustable PlayerIdentity where
  isExhausted = playerIdentityExhausted . toAttrs

instance HasResources PlayerIdentity where
  resourcesFor player card =
    concatMapM (`resourcesFor` card) (unHand $ playerIdentityHand $ toAttrs player)

instance IsSource PlayerIdentity where
  toSource = IdentitySource . playerIdentityId . toAttrs

instance IsTarget PlayerIdentity where
  toTarget = IdentityTarget . playerIdentityId . toAttrs

currentIdentity :: PlayerIdentity -> PlayerIdentitySide
currentIdentity (PlayerIdentity a) =
  case lookup (playerIdentitySide a) (playerIdentitySides a) of
    Nothing -> error "Should not happen"
    Just s -> s

createIdentity
  :: IdentityId -> PlayerIdentitySide -> PlayerIdentitySide -> PlayerIdentity
createIdentity ident alterEgoSide heroSide = PlayerIdentity $ PlayerIdentityAttrs
  { playerIdentitySide = B
  , playerIdentitySides = fromList [(A, heroSide), (B, alterEgoSide)]
  , playerIdentityId = ident
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
  }
 where
  hp = case alterEgoSide of
    AlterEgoSide x -> startingHP x
    HeroSide x -> startingHP x

setDeck :: Deck -> PlayerIdentity -> PlayerIdentity
setDeck deck (PlayerIdentity attrs) = PlayerIdentity $ attrs & deckL .~ deck'
 where
  ident = playerIdentityId attrs
  deck' = Deck $ map
    (\c -> c { pcOwner = Just ident, pcController = Just ident })
    (unDeck deck)

lookupAlterEgo :: CardDef -> IdentityId -> Maybe PlayerIdentitySide
lookupAlterEgo cardDef ident =
  Just . AlterEgoSide $ lookupAlterEgoByCardCode (toCardCode cardDef) ident

lookupHero :: CardDef -> IdentityId -> Maybe PlayerIdentitySide
lookupHero cardDef ident =
  Just . HeroSide $ lookupHeroByCardCode (toCardCode cardDef) ident

takeTurn :: MonadGame env m => PlayerIdentity -> m ()
takeTurn attrs =
  pushAll $ map (IdentityMessage $ toId attrs) [PlayerTurnOption, CheckIfPassed]

instance HasAbilities PlayerIdentity where
  getAbilities a =
    let
      sideAbilities = case currentIdentity a of
        HeroSide x -> getAbilities x
        AlterEgoSide x -> getAbilities x
    in
      [limitedAbility a 100 (PerTurn 1) Action IsSelf NoCost ChangeForm]
        <> sideAbilities

isPlayable :: MonadGame env m => PlayerIdentity -> PlayerCard -> m Bool
isPlayable attrs c = do
  resources <- getAvailableResourcesFor (Just c)
  modifiedCost <- getModifiedCost attrs c
  passedCriteria <- checkCriteria (cdCriteria def <> toAdditionalCriteria def)
  pure
    $ length resources
    >= modifiedCost
    && passedCriteria
    && isNothing (cdResponseWindow def)
    && (cdCardType def /= ResourceType)
 where
  ident = toId attrs
  def = getCardDef c
  checkCriteria = \case
    IsSelf -> error "Irrelevant"
    OwnsThis -> error "Irrelevant"
    NoCriteria -> pure True
    Never -> pure False
    SelfMatches identityMatcher ->
      member ident <$> select (IdentityWithId ident <> identityMatcher)
    InHeroForm -> member ident <$> select HeroIdentity
    InAlterEgoForm -> member ident <$> select AlterEgoIdentity
    Unexhausted -> member ident <$> select UnexhaustedIdentity
    Exhausted -> member ident <$> select Matchers.ExhaustedIdentity
    Criteria xs -> allM checkCriteria xs
    MinionExists m -> selectAny m
    EnemyExists m -> selectAny m
    CharacterExists m -> selectAny m
    AllyExists m -> selectAny m
    SchemeExists m -> selectAny m
    ExtendedCardExists m -> selectAny (NotCard c <> m)
    -- ^ this is critical and order matters to avoid infinite recursion

getModifiedCost :: MonadGame env m => PlayerIdentity -> PlayerCard -> m Int
getModifiedCost attrs c = do
  modifiers <- getModifiers attrs
  pure $ maybe 0 (modifiedCost modifiers) (cdCost $ getCardDef c)
 where
  modifiedCost ms cost' = foldr applyModifier cost' ms
  applyModifier (ResourceCostReduction n) = max 0 . subtract (fromIntegral n)
  applyModifier _ = id

getPlayableCards :: MonadGame env m => PlayerIdentity -> m [PlayerCard]
getPlayableCards player = filterM (isPlayable player) cards
  where cards = unHand $ playerIdentityHand $ toAttrs player

getChoices :: MonadGame env m => PlayerIdentity -> m [Choice]
getChoices attrs = do
  let ident = toId attrs
  abilities <- getsGame getAbilities
  usedAbilities <- getUsedAbilities
  playableCards <- getPlayableCards attrs
  validAbilities <- filterM
    (andM . sequence
      [ pure . passesUseLimit ident usedAbilities
      , passesCriteria ident
      , passesCanAffordCost ident
      , pure . passesTiming ident
      , passesTypeIsRelevant ident
      ]
    )
    abilities
  pure
    $ map UseAbility validAbilities
    <> map (($ Nothing) . PlayCard) playableCards

getObligations :: MonadRandom m => PlayerIdentity -> m [EncounterCard]
getObligations attrs = case currentIdentity attrs of
  HeroSide _ -> error "Can not call via hero side"
  AlterEgoSide side ->
    traverse genEncounterCard (alterEgoObligations $ toAttrs side)

toRetaliate :: [Keyword] -> Natural
toRetaliate [] = 0
toRetaliate (Retaliate n : _) = n
toRetaliate (_ : xs) = toRetaliate xs

runIdentityMessage
  :: MonadGame env m => IdentityMessage -> PlayerIdentity -> m PlayerIdentity
runIdentityMessage msg pid@(PlayerIdentity attrs@PlayerIdentityAttrs {..}) =
  case msg of
    SetupIdentity -> do
      let
        nemesisSet = getNemesisSet $ toCardCode pid
        setupAbilities = filter ((== Setup) . abilityType) $ getAbilities pid
      obligations <- getObligations pid
      nemesisSetCards <- gatherEncounterSet nemesisSet
      pushAll
        $ SetAside (EncounterCard <$> nemesisSetCards)
        : ShuffleIntoEncounterDeck obligations
        : map
            (IdentityMessage playerIdentityId)
            [ ShuffleDeck
            , DrawOrDiscardToHandLimit
            , DiscardCards
            , DrawOrDiscardToHandLimit
            ]
        <> map
             (\a -> RanAbility (toTarget pid) (abilityIndex a) [] NoPayment)
             setupAbilities
      pure pid
    BeginTurn -> do
      takeTurn pid
      pure $ PlayerIdentity $ attrs & passedL .~ False
    CheckIfPassed -> do
      unless playerIdentityPassed (takeTurn pid)
      pure pid
    PlayerTurnOption -> do
      choices <- getChoices pid
      chooseOne playerIdentityId (EndTurn : choices)
      pure pid
    EndedTurn -> do
      push $ IdentityEndedTurn playerIdentityId
      pure $ PlayerIdentity $ attrs & passedL .~ True
    ShuffleDeck -> do
      deck <- shuffleM (unDeck $ attrs ^. deckL)
      pure $ PlayerIdentity $ attrs & deckL .~ Deck deck
    ShuffleIdentityDiscardBackIntoDeck -> do
      deck' <- shuffleM
        (unDeck (attrs ^. deckL) <> unDiscard (attrs ^. discardL))
      pure
        $ PlayerIdentity
        $ attrs
        & (deckL .~ Deck deck')
        & (discardL .~ Discard [])
    ShuffleIntoIdentityDeck cards -> do
      deck <- shuffleM (unDeck (attrs ^. deckL) <> cards)
      pure $ PlayerIdentity $ attrs & deckL .~ Deck deck
    DrawOrDiscardToHandLimit -> do
      modifiedHandSize <- getModifiedHandSize pid
      let
        diff =
          fromIntegral modifiedHandSize - length (unHand playerIdentityHand)
      when (diff > 0) $ push $ IdentityMessage playerIdentityId $ DrawCards
        FromDeck
        (fromIntegral diff)
      when (diff < 0) $ chooseOne
        playerIdentityId
        [ TargetLabel
            (CardIdTarget $ pcCardId c)
            [ Run
                [ DiscardedCard $ PlayerCard c
                , IdentityMessage playerIdentityId DrawOrDiscardToHandLimit
                ]
            ]
        | c <- unHand playerIdentityHand
        ]

      pure pid
    DrawToHandLimit -> do
      modifiedHandSize <- getModifiedHandSize pid
      let
        diff =
          fromIntegral modifiedHandSize - length (unHand playerIdentityHand)
      when (diff > 0) $ push $ IdentityMessage playerIdentityId $ DrawCards
        FromDeck
        (fromIntegral diff)
      pure pid
    DrawCards fromZone n -> case fromZone of
      FromHand -> error "Impossible"
      FromDiscard -> error "Impossible"
      RandomFromHand -> error "Impossible"
      FromEncounterDeck -> error "Impossible"
      FromDeck -> do
        let
          (cards, deck) = splitAt (fromIntegral n) (unDeck playerIdentityDeck)
        when (length (unDeck playerIdentityDeck) < fromIntegral n) $ pushAll
          [ IdentityMessage playerIdentityId ShuffleIdentityDiscardBackIntoDeck
          , DealEncounterCard playerIdentityId
          , IdentityMessage playerIdentityId $ DrawCards
            fromZone
            (subtractNatural
              (fromIntegral (length (unDeck playerIdentityDeck)))
              n
            )
          ]
        pure
          $ PlayerIdentity
          $ attrs
          & handL
          %~ Hand
          . (<> cards)
          . unHand
          & deckL
          .~ Deck deck
    ReadyCards -> do
      pushAll
        $ map
            (($ ReadiedAlly) . AllyMessage)
            (HashSet.toList playerIdentityAllies)
        <> map
             (($ ReadiedSupport) . SupportMessage)
             (HashSet.toList playerIdentitySupports)
        <> map
             (($ ReadiedUpgrade) . UpgradeMessage)
             (HashSet.toList playerIdentityUpgrades)
      pure $ PlayerIdentity $ attrs & exhaustedL .~ False
    ChooseOtherForm -> do
      let
        otherForms = filter (/= playerIdentitySide) $ keys playerIdentitySides
      chooseOrRunOne playerIdentityId $ map ChangeToForm otherForms
      pure pid
    ChangedToForm side -> do
      push $ CheckWindows
        [W.Window W.After $ W.IdentityChangesForm playerIdentityId]
      pure $ PlayerIdentity $ attrs & sideL .~ side
    PlayedCard card mWindow -> do
      modifiedCost <- getModifiedCost pid card
      let cost' = mconcat $ replicate modifiedCost (ResourceCost Nothing)
      push $ SetActiveCost $ ActiveCost
        playerIdentityId
        (ForCard card)
        cost'
        NoPayment
        mWindow
        mempty
      pure
        $ PlayerIdentity
        $ attrs
        & (handL %~ Hand . filter (/= card) . unHand)
        & (discardL %~ Discard . filter (/= card) . unDiscard)
    PaidWithCard card -> do
      push $ Spent card
      pure
        $ PlayerIdentity
        $ attrs
        & (handL %~ Hand . filter (/= card) . unHand)
        -- & (discardL %~ Discard . (card :) . unDiscard)
    AllyCreated allyId -> do
      push $ IdentityMessage playerIdentityId CheckAllyLimit
      pure $ PlayerIdentity $ attrs & alliesL %~ HashSet.insert allyId
    CheckAllyLimit -> do
      limit <- fromIntegral <$> getModifiedAllyLimit pid
      when (size playerIdentityAllies > limit) $ chooseOne
        playerIdentityId
        [ TargetLabel (AllyTarget aid) [DiscardTarget $ AllyTarget aid]
        | aid <- toList playerIdentityAllies
        ]
      pure pid
    UpgradeCreated upgradeId -> do
      pure $ PlayerIdentity $ attrs & upgradesL %~ HashSet.insert upgradeId
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
      pure
        $ PlayerIdentity
        $ attrs
        & (handL %~ Hand . (card :) . unHand)
        & (deckL %~ Deck . filter (/= card) . unDeck)
        & (discardL %~ Discard . filter (/= card) . unDiscard)
    ChooseFromDiscard target choiceRules chooseMin chooseMax -> do
      pushAll
        [ FocusCards $ PlayerCard <$> unDiscard playerIdentityDiscard
        , IdentityMessage playerIdentityId
          $ ChosenFromDiscard target choiceRules chooseMin chooseMax []
        , UnfocusCards
        ]
      pure pid
    ChosenFromDiscard target choiceRules chooseMin chooseMax cards -> do
      let
        discards = unDiscard playerIdentityDiscard
        focusedCards = filter (`notElem` cards) discards
        chosenNames = map (cdName . pcCardDef) cards
        choices = case choiceRules of
          DifferentCards ->
            filter ((`notElem` chosenNames) . cdName . pcCardDef) discards

      if length cards >= fromIntegral chooseMax
        then push (WithChosen target FromDiscard $ PlayerCard <$> cards)
        else pushAll
          [ FocusCards $ PlayerCard <$> focusedCards
          , Ask playerIdentityId
          $ ChooseOne
          $ [ Label
                "Done"
                [Run [WithChosen target FromDiscard $ PlayerCard <$> cards]]
            | length cards >= fromIntegral chooseMin
            ]
          <> [ TargetLabel
                 (CardIdTarget $ pcCardId c)
                 [ Run
                     [ IdentityMessage playerIdentityId $ ChosenFromDiscard
                         target
                         choiceRules
                         chooseMin
                         chooseMax
                         (c : cards)
                     ]
                 ]
             | c <- choices
             ]
          ]

      pure pid
    DiscardFor _ FromDeck _ _ -> error "Unhandled"
    DiscardedFor _ FromDeck _ _ _ -> error "Unhandled"
    DiscardFor _ FromEncounterDeck _ _ -> error "Unhandled"
    DiscardedFor _ FromEncounterDeck _ _ _ -> error "Unhandled"
    DiscardFor _ FromDiscard _ _ -> error "Unhandled"
    DiscardedFor _ FromDiscard _ _ _ -> error "Unhandled"
    DiscardFor target FromHand discardMin discardMax -> do
      push $ IdentityMessage playerIdentityId $ DiscardedFor
        target
        FromHand
        discardMin
        discardMax
        []
      pure pid
    DiscardedFor target FromHand discardMin discardMax cards -> do
      if length cards >= fromIntegral discardMax
        then push (WithDiscarded target FromHand cards)
        else
          push
          $ Ask playerIdentityId
          $ ChooseOne
          $ [ Label "Done" [Run [WithDiscarded target FromHand cards]]
            | length cards >= fromIntegral discardMin
            ]
          <> [ TargetLabel
                 (CardIdTarget $ pcCardId c)
                 [ Run
                     [ DiscardedCard $ PlayerCard c
                     , IdentityMessage playerIdentityId $ DiscardedFor
                       target
                       FromHand
                       discardMin
                       discardMax
                       (PlayerCard c : cards)
                     ]
                 ]
             | c <- unHand playerIdentityHand
             ]
      pure pid
    DiscardFor target RandomFromHand discardMin _ -> do
      let handCards = unHand playerIdentityHand
      discards <- take (fromIntegral discardMin) <$> shuffleM handCards
      pushAll
        $ map (DiscardedCard . PlayerCard) discards
        <> [WithDiscarded target RandomFromHand $ PlayerCard <$> discards]
      pure pid
    DiscardedFor _ RandomFromHand _ _ _ -> error "Can not be called"
    DiscardCards -> do
      unless (null $ unHand playerIdentityHand) $ do
        chooseOne playerIdentityId
          $ Label "Continue without discarding" []
          : [ TargetLabel
                (CardIdTarget $ pcCardId c)
                [ Run
                    [ DiscardedCard $ PlayerCard c
                    , IdentityMessage playerIdentityId DiscardCards
                    ]
                ]
            | c <- unHand playerIdentityHand
            ]
      pure pid
    DiscardFrom fromZone n mTarget -> case fromZone of
      FromHand -> error "Unhandled"
      FromDiscard -> error "Unhandled"
      FromEncounterDeck -> error "Unhandled"
      RandomFromHand -> do
        discards <- take (fromIntegral n)
          <$> shuffleM (unHand playerIdentityHand)
        for_ mTarget $ \target ->
          push $ WithDiscarded target fromZone $ PlayerCard <$> discards
        pure
          $ PlayerIdentity
          $ attrs
          & (discardL %~ Discard . (discards <>) . unDiscard)
          & (handL %~ Hand . filter (`notElem` discards) . unHand)
      FromDeck -> do
        let
          (cards, deck') = splitAt (fromIntegral n) $ unDeck playerIdentityDeck
        for_ mTarget $ \target ->
          push $ WithDiscarded target fromZone $ PlayerCard <$> cards
        pure
          $ PlayerIdentity
          $ attrs
          & (discardL %~ Discard . (cards <>) . unDiscard)
          & (deckL .~ Deck deck')
    ReadiedIdentity -> pure $ PlayerIdentity $ attrs & exhaustedL .~ False
    ExhaustedIdentity -> pure $ PlayerIdentity $ attrs & exhaustedL .~ True
    VillainAndMinionsActivate -> do
      villain <- selectJust ActiveVillain
      case currentIdentity pid of
        HeroSide _ -> do
          pushAll
            $ VillainMessage villain (VillainAttacks playerIdentityId)
            : [ Ask playerIdentityId $ ChooseOneAtATime $ map
                  (\minionId -> TargetLabel
                    (MinionTarget minionId)
                    [ Run
                        [ MinionMessage
                            minionId
                            (MinionAttacks playerIdentityId)
                        ]
                    ]
                  )
                  (toList playerIdentityMinions)
              | not (null playerIdentityMinions)
              ]
        AlterEgoSide _ -> do
          pushAll
            $ VillainMessage villain VillainSchemes
            : [ Ask playerIdentityId $ ChooseOneAtATime $ map
                  (\minionId -> TargetLabel
                    (MinionTarget minionId)
                    [Run [MinionMessage minionId MinionSchemes]]
                  )
                  (toList playerIdentityMinions)
              | not (null playerIdentityMinions)
              ]
      pure pid
    DealtEncounterCard ec -> pure $ PlayerIdentity $ attrs & encounterCardsL %~ (ec :)
    RevealEncounterCards -> do
      pushAll $ map
        (RevealEncounterCard playerIdentityId)
        playerIdentityEncounterCards
      pure $ PlayerIdentity $ attrs & encounterCardsL .~ mempty
    IdentityWasAttacked attack' -> do
      let
        damage =
          subtractNatural playerIdentityDamageReduction $ attackDamage attack'
        retaliate = case currentIdentity pid of
          HeroSide x -> toRetaliate . toList . cdKeywords $ getCardDef x
          AlterEgoSide x -> toRetaliate . toList . cdKeywords $ getCardDef x
      when (retaliate > 0) $ push $ IdentityMessage
        playerIdentityId
        (IdentityRetaliate retaliate $ attackEnemy attack')
      when playerIdentityDefended $ push
        (CheckWindows
          [ W.Window W.After
              $ W.HeroDefends playerIdentityId (attackEnemy attack')
          ]
        )
      when (damage > 0) $ pushAll
        [ CheckWindows
          [ W.Window W.When $ W.IdentityTakeDamage
              playerIdentityId
              (toDamage damage FromAttack)
          ]
        , IdentityMessage playerIdentityId
          $ IdentityDamaged (attackSource attack') (toDamage damage FromAttack)
        ]
      pure $ PlayerIdentity $ attrs & damageReductionL .~ 0 & defendedL .~ False
    IdentityDamaged _ damage -> do
      modifiedHp <- getModifiedHp pid
      when
        (playerIdentityDamage + damageAmount damage >= modifiedHp)
        (push $ IdentityMessage playerIdentityId IdentityDefeated)
      pure $ PlayerIdentity $ attrs & damageL +~ damageAmount damage
    IdentityDefeated -> do
      players <- getPlayers
      let
        ps = take 1 . drop 1 $ dropWhile (/= playerIdentityId) (cycle players)
      push (RemoveFromPlay $ IdentityTarget playerIdentityId)
      case ps of
        [x] | x /= playerIdentityId -> pushAll $ concatMap
          (\minionId ->
            [ IdentityMessage x (MinionEngaged minionId)
            , MinionMessage minionId (MinionEngagedIdentity x)
            ]
          )
          (toList playerIdentityMinions)
        _ -> pure ()
      pure $ PlayerIdentity $ attrs & defeatedL .~ True
    IdentityDefended n ->
      pure $ PlayerIdentity $ attrs & damageReductionL +~ n & defendedL .~ True
    IdentityHealed n ->
      pure $ PlayerIdentity $ attrs & damageL %~ subtractNatural n
    IdentityStunned -> pure $ PlayerIdentity $ attrs & stunnedL .~ True
    IdentityConfused -> pure $ PlayerIdentity $ attrs & confusedL .~ True
    IdentityRemoveStunned -> pure $ PlayerIdentity $ attrs & stunnedL .~ False
    IdentityRemoveConfused ->
      pure $ PlayerIdentity $ attrs & confusedL .~ False
    Search (SearchIdentityDeck iid projection) cardMatcher searchOption returnOption
      | iid == playerIdentityId
      -> do
        let
          deck = unDeck playerIdentityDeck
          (focusedCards, deck') = case projection of
            AllOfDeck -> (deck, [])
            TopOfDeck n -> splitAt n deck
          (foundCards, rest) = partition (cardMatch cardMatcher) focusedCards
          handleReturnCards = case returnOption of
            ShuffleBackIn ->
              (: [])
                . IdentityMessage playerIdentityId
                . ShuffleIntoIdentityDeck
            DiscardRest -> map (DiscardedCard . PlayerCard)
          handleFoundCards = if null foundCards
            then Ask
              playerIdentityId
              (ChooseOne [Label "No matching cards found" []])
            else case searchOption of
              SearchTarget target ->
                SearchFoundCards target $ PlayerCard <$> foundCards
              SearchDrawOne -> Ask playerIdentityId $ ChooseOne
                [ TargetLabel
                    (CardIdTarget $ pcCardId c)
                    [ Run
                      $ IdentityMessage playerIdentityId (AddToHand c)
                      : handleReturnCards (rest <> cs')
                    ]
                | (c, cs') <- removeEach foundCards
                ]
        pushAll
          [ FocusCards $ PlayerCard <$> focusedCards
          , handleFoundCards
          , UnfocusCards
          ]
        pure $ PlayerIdentity $ attrs & deckL .~ Deck deck'
    Search _ _ _ _ -> error "Unhandled"
    IdentityRetaliate n enemyId -> do
      let
        target = case enemyId of
          EnemyMinionId mid -> MinionTarget mid
          EnemyVillainId vid -> VillainTarget vid
      msgs <- choiceMessages playerIdentityId
        $ DamageEnemy target (toSource pid) (toDamage n FromRetaliate)
      pushAll msgs
      pure pid
    SideMessage _ -> case currentIdentity pid of
      HeroSide x -> do
        newSide <-
          HeroSide <$> runMessage (IdentityMessage playerIdentityId msg) x
        pure
          $ PlayerIdentity
          $ attrs
          & sidesL
          . at playerIdentitySide
          ?~ newSide
      AlterEgoSide x -> do
        newSide <-
          AlterEgoSide <$> runMessage (IdentityMessage playerIdentityId msg) x
        pure
          $ PlayerIdentity
          $ attrs
          & sidesL
          . at playerIdentitySide
          ?~ newSide

instance RunMessage PlayerIdentity where
  runMessage msg pid@(PlayerIdentity attrs) = case msg of
    DiscardedCard (PlayerCard card)
      | pcOwner card == Just (playerIdentityId attrs)
      -> pure
        $ PlayerIdentity
        $ attrs
        & (discardL %~ Discard . (card :) . unDiscard)
        & (handL %~ Hand . filter (/= card) . unHand)
    RemoveFromGame (CardIdTarget cid) ->
      pure
        $ PlayerIdentity
        $ attrs
        & (discardL %~ Discard . filter ((/= cid) . pcCardId) . unDiscard)
        & (handL %~ Hand . filter ((/= cid) . pcCardId) . unHand)
        & (deckL %~ Deck . filter ((/= cid) . pcCardId) . unDeck)
    UpgradeRemoved upgradeId -> do
      pure $ PlayerIdentity $ attrs & upgradesL %~ HashSet.delete upgradeId
    IdentityMessage ident msg' | ident == playerIdentityId attrs ->
      runIdentityMessage msg' pid
    _ -> case currentIdentity pid of
      HeroSide x -> do
        newSide <- HeroSide <$> runMessage msg x
        pure
          $ PlayerIdentity
          $ attrs
          & sidesL
          . at (playerIdentitySide attrs)
          ?~ newSide
      AlterEgoSide x -> do
        newSide <- AlterEgoSide <$> runMessage msg x
        pure
          $ PlayerIdentity
          $ attrs
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

getModifiedAllyLimit :: MonadGame env m => PlayerIdentity -> m Natural
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
