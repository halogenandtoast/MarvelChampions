{-# LANGUAGE TemplateHaskell #-}
module Marvel.Game where

import Marvel.Prelude

import Data.Aeson.Diff qualified as Diff
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (maximum)
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Ally
import Marvel.Ally.Types (getAllyController, getAllyDamage, getAllyUses)
import Marvel.AlterEgo.Cards
import Marvel.Attachment
import Marvel.Attachment.Types (Attachment)
import Marvel.Attack
import Marvel.Card
import Marvel.Criteria
import Marvel.Debug
import Marvel.Deck
import Marvel.Difficulty
import Marvel.Discard
import Marvel.Effect
import Marvel.Effect.Types (Effect)
import Marvel.EncounterCard
import Marvel.EncounterSet
import Marvel.Entity
import Marvel.Event
import Marvel.Event.Types (Event)
import Marvel.Exception
import Marvel.Hand
import Marvel.Hero.Cards
import Marvel.Hp
import Marvel.Id
import Marvel.Identity hiding (alliesL, minionsL, supportsL, upgradesL)
import Marvel.Keyword
import Marvel.MainScheme
import Marvel.MainScheme.Types (MainScheme, getMainSchemeThreat)
import Marvel.Matchers hiding (ExhaustedIdentity, MainScheme)
import Marvel.Matchers qualified as Matchers
import Marvel.Message hiding (ExhaustedAlly)
import Marvel.Minion
import Marvel.Minion.Types
  ( Minion
  , getMinionDamage
  , getMinionEngagedIdentity
  , getMinionPrintedHitPoints
  , minionAttackDetails
  )
import Marvel.Modifier
import Marvel.Obligation
import Marvel.Obligation.Types (Obligation)
import Marvel.Phase
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Scenario
import Marvel.Scenario.Types (Scenario, getScenarioDifficulty)
import Marvel.SideScheme
import Marvel.SideScheme.Types (SideScheme, isCrisis, getSideSchemeThreat)
import Marvel.Source
import Marvel.Support
import Marvel.Support.Types (Support, getSupportController, getSupportUses)
import Marvel.Target
import Marvel.Trait
import Marvel.Treachery
import Marvel.Treachery.Types (Treachery)
import Marvel.Upgrade
import Marvel.Upgrade.Types (Upgrade, getUpgradeUses, getUpgradeController)
import Marvel.Villain
import Marvel.Villain.Types (Villain, villainDamage, villainIsTough, villainAttackDetails)
import Marvel.Window (Window, WindowTiming(..), windowMatches)
import Marvel.Window qualified as W

data GameState = Unstarted | InProgress | Finished FinishedStatus
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

type EntityMap a = HashMap (EntityId a) a

data Entities = Entities
  { entitiesPlayers :: EntityMap PlayerIdentity
  , entitiesVillains :: EntityMap Villain
  , entitiesMinions :: EntityMap Minion
  , entitiesAllies :: EntityMap Ally
  , entitiesAttachments :: EntityMap Attachment
  , entitiesSupports :: EntityMap Support
  , entitiesUpgrades :: EntityMap Upgrade
  , entitiesTreacheries :: EntityMap Treachery
  , entitiesObligations :: EntityMap Obligation
  , entitiesMainSchemes :: EntityMap MainScheme
  , entitiesSideSchemes :: EntityMap SideScheme
  , entitiesEffects :: EntityMap Effect
  , entitiesEvents :: EntityMap Event
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''Entities

defaultEntities :: Entities
defaultEntities = Entities
  { entitiesPlayers = mempty
  , entitiesVillains = mempty
  , entitiesMinions = mempty
  , entitiesAllies = mempty
  , entitiesAttachments = mempty
  , entitiesSupports = mempty
  , entitiesUpgrades = mempty
  , entitiesTreacheries = mempty
  , entitiesObligations = mempty
  , entitiesSideSchemes = mempty
  , entitiesMainSchemes = mempty
  , entitiesEffects = mempty
  , entitiesEvents = mempty
  }

data Game = Game
  { gamePhase :: Phase
  , gameState :: GameState
  , -- players in player order
    gamePlayerOrder :: [IdentityId]
  , gameEntities :: Entities
  , gameBoostEntities :: Entities
  , gameRemovedEntities :: Entities
  , gameQuestion :: HashMap IdentityId Question
  , gameUsedAbilities :: HashMap IdentityId [(Ability, Int)]
  , gameActivePlayer :: IdentityId
  , gameActiveCost :: Maybe ActiveCost
  , gameWindowDepth :: Int
  , gameWindows :: [[Window]]
  , gameScenario :: Scenario
  , gameFocusedCards :: [Card]
  , gameBoostCards :: HashMap Target EncounterCard
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''Game

gameAllies :: Game -> EntityMap Ally
gameAllies = entitiesAllies . gameEntities

gameAttachments :: Game -> EntityMap Attachment
gameAttachments = entitiesAttachments . gameEntities

gameEffects :: Game -> EntityMap Effect
gameEffects = entitiesEffects . gameEntities

gameEvents :: Game -> EntityMap Event
gameEvents = entitiesEvents . gameEntities

gameMinions :: Game -> EntityMap Minion
gameMinions = entitiesMinions . gameEntities

gamePlayers :: Game -> EntityMap PlayerIdentity
gamePlayers = entitiesPlayers . gameEntities

gameUpgrades :: Game -> EntityMap Upgrade
gameUpgrades = entitiesUpgrades . gameEntities

gameSupports :: Game -> EntityMap Support
gameSupports = entitiesSupports . gameEntities

gameMainSchemes :: Game -> EntityMap MainScheme
gameMainSchemes = entitiesMainSchemes . gameEntities

gameSideSchemes :: Game -> EntityMap SideScheme
gameSideSchemes = entitiesSideSchemes . gameEntities

gameTreacheries :: Game -> EntityMap Treachery
gameTreacheries = entitiesTreacheries . gameEntities

gameObligations :: Game -> EntityMap Obligation
gameObligations = entitiesObligations . gameEntities

gameVillains :: Game -> EntityMap Villain
gameVillains = entitiesVillains . gameEntities

instance ToJSON Game where
  toJSON = genericToJSON $ aesonOptions $ Just "game"

instance FromJSON Game where
  parseJSON = genericParseJSON $ aesonOptions $ Just "game"

diff :: Game -> Game -> Diff.Patch
diff a b = Diff.diff (toJSON a) (toJSON b)

patch :: Game -> Diff.Patch -> Result Game
patch g p = case Diff.patch p (toJSON g) of
  Error e -> Error e
  Success a -> fromJSON a

getPlayers :: MonadGame env m => m [IdentityId]
getPlayers = getsGame gamePlayerOrder

getActivePlayerId :: MonadGame env m => m IdentityId
getActivePlayerId = toId <$> getActivePlayer

getPlayerCount :: MonadGame env m => m Int
getPlayerCount = length <$> getsGame gamePlayerOrder

getActivePlayer :: MonadGame env m => m PlayerIdentity
getActivePlayer = getsGame $ \g ->
  case lookup (g ^. activePlayerL) (g ^. entitiesL . playersL) of
    Just p -> p
    Nothing -> error "Missing player"

runPreGameMessage :: MonadGame env m => Message -> Game -> m Game
runPreGameMessage msg g = case msg of
  CheckWindows windows -> do
    push EndCheckWindows
    pure $ g & windowDepthL +~ 1 & windowsL %~ (windows :)
  -- We want to empty the queue for triggering a resolution
  EndCheckWindows ->
    pure
      $ g
      & (windowDepthL -~ 1)
      & (windowsL %~ drop 1)
      & (usedAbilitiesL
        . each
        %~ filter
             (\(a, d) ->
               (abilityLimit a /= PerWindow 1) || d < gameWindowDepth g
             )
        )
  _ -> pure g

runGameMessage :: MonadGame env m => Message -> Game -> m Game
runGameMessage msg g@Game {..} = case msg of
  StartGame -> do
    push StartScenario
    pushAll $ map (`IdentityMessage` SetupIdentity) gamePlayerOrder
    case gamePlayerOrder of
      [] -> throwM NoPlayers
      [p] -> push (SetPlayerOrder [p])
      players@(p : _) -> choosePlayerOrder p players
    pure $ g { gameState = InProgress }
  SetPlayerOrder xs -> pure $ g { gamePlayerOrder = xs }
  RemoveFromGame target -> case target of
    ObligationTarget oid -> pure $ g & (entitiesL . obligationsL %~ delete oid)
    CardIdTarget _ -> pure g -- Should be handled by Identity
    _ -> error "Unhandled"
  RemoveFromPlay target -> case target of
    IdentityTarget iid -> do
      when (length (g ^. entitiesL . playersL) <= 1) (push $ GameOver Lost)
      pure
        $ g
        & (entitiesL . playersL %~ delete iid)
        & (playerOrderL %~ filter (/= iid))
    AllyTarget aid -> do
      for_ (lookup aid $ gameAllies g) $ \ally -> do
        let ident = getAllyController ally
        pushAll
          [IdentityMessage ident $ AllyRemoved aid, DiscardedCard $ toCard ally]
      pure $ g & (entitiesL . alliesL %~ delete aid)
    SupportTarget sid -> do
      for_ (lookup sid $ gameSupports g) $ \support ->
        push $ IdentityMessage (getSupportController support) $ SupportRemoved
          sid
      pure $ g & (entitiesL . supportsL %~ delete sid)
    UpgradeTarget uid -> do
      for_ (lookup uid $ gameUpgrades g) $ \upgrade ->
        pushAll [UpgradeRemoved uid, DiscardedCard $ toCard upgrade]
      pure $ g & (entitiesL . upgradesL %~ delete uid)
    AttachmentTarget aid -> do
      for_ (lookup aid $ gameAttachments g) $ \attachment ->
        pushAll [AttachmentRemoved aid, DiscardedCard $ toCard attachment]
      pure $ g & (entitiesL . attachmentsL %~ delete aid)
    MinionTarget mid -> case lookup mid (gameMinions g) of
      Nothing -> pure g
      Just minion -> do
        push $ DiscardedCard $ toCard minion
        pure
          $ g
          & (entitiesL . minionsL %~ delete mid)
          & (removedEntitiesL . minionsL %~ insert mid minion)
    TreacheryTarget tid -> do
      for_ (lookup tid $ gameTreacheries g)
        $ \treachery -> push $ DiscardedCard $ toCard treachery
      pure $ g & (entitiesL . treacheriesL %~ delete tid)
    ObligationTarget oid -> do
      for_ (lookup oid $ gameObligations g)
        $ \obligation -> push $ DiscardedCard $ toCard obligation
      pure $ g & (entitiesL . obligationsL %~ delete oid)
    SideSchemeTarget sid -> do
      for_ (lookup sid $ gameSideSchemes g)
        $ \sideScheme -> push $ DiscardedCard $ toCard sideScheme
      pure $ g & (entitiesL . sideSchemesL %~ delete sid)
    _ -> error "Unhandled target"
  AddMainScheme cardCode -> do
    mainSchemeId <- getRandom
    case lookupMainScheme cardCode mainSchemeId of
      Just x -> do
        push $ MainSchemeMessage mainSchemeId RevealMainScheme
        pure $ g & (entitiesL . mainSchemesL . at mainSchemeId ?~ x)
      Nothing -> throwM $ MissingCardCode "AddMainScheme" cardCode
  ReplaceMainScheme cardCode -> do
    push (AddMainScheme cardCode)
    pure $ g & (entitiesL . mainSchemesL .~ mempty)
  AddVillain cardCode -> do
    villainId <- getRandom
    case lookupVillain cardCode villainId of
      Just x -> do
        push $ VillainMessage villainId SetVillainHp
        pure $ g & (entitiesL . villainsL . at villainId ?~ x)
      Nothing -> throwM $ MissingCardCode "AddVillain" cardCode
  UsedAbility ident a ->
    pure $ g & usedAbilitiesL %~ insertWith (<>) ident [(a, gameWindowDepth)]
  SetActiveCost activeCost -> do
    cards <- getAvailablePaymentSources
    abilities <- getResourceAbilities
    paid <- resourceCostPaid activeCost
    push $ Ask
      (activeCostIdentityId activeCost)
      (ChooseOne
      $ map PayWithCard cards
      <> map UseAbility abilities
      <> [ FinishPayment | paid ]
      )
    pure $ g & activeCostL ?~ activeCost
  Spent discard -> case g ^. activeCostL of
    Just activeCost -> do
      let
        activeCost' = activeCost
          { activeCostSpentCards = discard : activeCostSpentCards activeCost
          }
      case activeCostTarget activeCost of
        ForCard card -> do
          resources <- resourcesFor discard $ Just card
          push $ Paid $ mconcat $ map ResourcePayment resources
          pure $ g & activeCostL ?~ activeCost'
        ForAbility _ -> do
          resources <- resourcesFor discard Nothing
          push $ Paid $ mconcat $ map ResourcePayment resources
          pure $ g & activeCostL ?~ activeCost'
        ForTreachery -> do
          resources <- resourcesFor discard Nothing
          push $ Paid $ mconcat $ map ResourcePayment resources
          pure $ g & activeCostL ?~ activeCost'
    Nothing -> error "No active cost"
  Paid payment -> case g ^. activeCostL of
    Just activeCost -> do
      let
        activeCost' = activeCost
          { activeCostPayment = activeCostPayment activeCost <> payment
          }
      cards <- getAvailablePaymentSources
      abilities <- getResourceAbilities
      paid <- resourceCostPaid activeCost'
      push
        $ Ask (activeCostIdentityId activeCost)
        $ ChooseOne
        $ map PayWithCard cards
        <> map UseAbility abilities
        <> [ FinishPayment | paid ]
      pure $ g & activeCostL ?~ activeCost'
    Nothing -> error "No cost"
  FinishedPayment -> case g ^. activeCostL of
    Just activeCost -> do
      case activeCostTarget activeCost of
        ForCard card -> do
          push $ PutCardIntoPlay
            (activeCostIdentityId activeCost)
            card
            (activeCostPayment activeCost)
            (activeCostWindow activeCost)
        ForAbility _ -> pure ()
        ForTreachery -> pure ()
      pushAll $ map
        (DiscardedCard . PlayerCard)
        (reverse $ activeCostSpentCards activeCost)
      pure $ g & activeCostL .~ Nothing
    Nothing -> error "no active cost"
  CreatedEffect def source matcher -> do
    effectId <- getRandom
    ident <- toId <$> getActivePlayer
    let effect = createEffect effectId (toCardCode def) source matcher
    push $ EffectMessage effectId (UsedEffect ident)
    pure $ g & (entitiesL . effectsL %~ insert effectId effect)
  DisabledEffect effectId ->
    pure $ g & (entitiesL . effectsL %~ delete effectId)
  PutCardIntoPlay ident card payment mWindow -> do
    case cdCardType (getCardDef card) of
      AllyType -> do
        let ally = createAlly ident card
        pushAll
          [ IdentityMessage ident (AllyCreated $ toId ally)
          , CheckWindows [W.Window After $ W.PlayedAlly (toId ally)]
          ]
        pure $ g & (entitiesL . alliesL %~ insert (toId ally) ally)
      SupportType -> do
        let support = createSupport ident card
        pushAll
          [ IdentityMessage ident (SupportCreated $ toId support)
          , CheckWindows [W.Window After $ W.PlayedSupport (toId support)]
          ]
        pure $ g & (entitiesL . supportsL %~ insert (toId support) support)
      UpgradeType -> do
        let upgrade = createUpgrade ident card
        push $ UpgradeMessage (toId upgrade) PlayedUpgrade
        pure $ g & (entitiesL . upgradesL %~ insert (toId upgrade) upgrade)
      EventType -> do
        let
          event = createEvent ident card
          mSubType = cdAbilitySubType $ getCardDef card
          playEvent = EventMessage
            (toId event)
            (PlayedEvent ident payment (W.windowType <$> mWindow))
        playedMessage <- case mSubType of
          Just Ability.Attack -> do
            stunned <- identityMatches StunnedIdentity ident
            if stunned
              then pure (IdentityMessage ident IdentityRemoveStunned)
              else pure playEvent
          Just Ability.Thwart -> do
            confused <- identityMatches ConfusedIdentity ident
            if confused
              then pure (IdentityMessage ident IdentityRemoveConfused)
              else pure playEvent
          _ -> pure playEvent
        pushAll [playedMessage, EventMessage (toId event) ResolvedEvent]
        pure $ g & (entitiesL . eventsL %~ insert (toId event) event)
      _ -> error "Unhandled"
  PutBoostIntoPlay target ident -> case target of
    MinionTarget minionId ->
      case g ^. boostEntitiesL . minionsL . at minionId of
        Just minion -> do
          pushAll
            [ IdentityMessage ident $ MinionEngaged minionId
            , MinionMessage minionId $ MinionEngagedIdentity ident
            ]
          pure
            $ g
            & (boostEntitiesL . minionsL %~ delete (toId minion))
            & (entitiesL . minionsL %~ insert (toId minion) minion)
            & (boostCardsL %~ delete target)
        _ -> error "messed up somewhere"
    _ -> error "Unhandled"
  RevealBoostCard card enemyId -> do
    player <- getActivePlayerId
    case cdCardType (getCardDef card) of
      TreacheryType -> do
        let treachery = createTreachery card
        push (RevealedAsBoost (toTarget treachery) enemyId)
        pure
          $ g
          & (boostEntitiesL . treacheriesL %~ insert (toId treachery) treachery)
          & (boostCardsL . at (toTarget treachery) ?~ card)
      MinionType -> do
        let minion = createMinion player card
        push (RevealedAsBoost (toTarget minion) enemyId)
        pure
          $ g
          & (boostEntitiesL . minionsL %~ insert (toId minion) minion)
          & (boostCardsL . at (toTarget minion) ?~ card)
      AttachmentType -> do
        let attachment = createAttachment card
        push (RevealedAsBoost (toTarget attachment) enemyId)
        pure
          $ g
          & (boostEntitiesL
            . attachmentsL
            %~ insert (toId attachment) attachment
            )
          & (boostCardsL . at (toTarget attachment) ?~ card)
      ObligationType -> do
        let obligation = createObligation card
        push (RevealedAsBoost (toTarget obligation) enemyId)
        pure
          $ g
          & (boostEntitiesL
            . obligationsL
            %~ insert (toId obligation) obligation
            )
          & (boostCardsL . at (toTarget obligation) ?~ card)
      SideSchemeType -> do
        let sideScheme = createSideScheme card
        push (RevealedAsBoost (toTarget sideScheme) enemyId)
        pure
          $ g
          & (boostEntitiesL
            . sideSchemesL
            %~ insert (toId sideScheme) sideScheme
            )
          & (boostCardsL . at (toTarget sideScheme) ?~ card)
      _ -> pure g
  ClearBoosts -> do
    pushAll $ map (DiscardedCard . EncounterCard) (elems gameBoostCards)
    pure $ g & boostEntitiesL .~ defaultEntities & boostCardsL .~ mempty
  ClearRemoved -> pure $ g & removedEntitiesL .~ defaultEntities
  RevealEncounterCard ident card -> do
    pushAll
      [ FocusCards [EncounterCard card]
      , CheckWindows [W.Window When $ W.EncounterCardRevealed ident card]
      , UnfocusCards
      , RevealedEncounterCard ident card
      ]
    pure g
  RevealedEncounterCard ident card -> do
    case cdCardType (getCardDef card) of
      AttachmentType -> do
        let attachment = createAttachment card
        pushAll [AttachmentMessage (toId attachment) $ RevealAttachment ident]
        pure
          $ g
          & (entitiesL . attachmentsL %~ insert (toId attachment) attachment)
      MinionType -> do
        let
          minion = createMinion ident card
          isTough = Toughness `member` cdKeywords (getCardDef card)
        pushAll
          $ [ MinionMessage (toId minion) MinionBecomeTough | isTough ]
          <> [ FocusCards [toCard minion]
             , CheckWindows [W.Window When $ W.MinionEnteredPlay $ toId minion]
             , MinionMessage (toId minion) (RevealMinion ident)
             , UnfocusCards
             , IdentityMessage ident (MinionEngaged $ toId minion)
             , MinionMessage (toId minion) (MinionEngagedIdentity ident)
             , CheckWindows [W.Window After $ W.MinionEnteredPlay $ toId minion]
             ]
        pure $ g & (entitiesL . minionsL %~ insert (toId minion) minion)
      TreacheryType -> do
        let treachery = createTreachery card
        -- TODO: FOCUS CARDS SHOULD BE CARDS...
        pushAll
          [ FocusCards [toCard treachery]
          , CheckWindows
            [ W.Window When
                $ W.RevealTreachery (toId treachery) W.RevealedFromEncounterDeck
            ]
          , TreacheryMessage (toId treachery) $ RevealTreachery ident
          , TreacheryMessage (toId treachery) $ CheckTreacheryCondition ident
          , UnfocusCards
          , TreacheryMessage (toId treachery) $ ResolvedTreachery ident
          ]
        pure
          $ g
          & (entitiesL . treacheriesL %~ insert (toId treachery) treachery)
      ObligationType -> do
        let obligation = createObligation card
        -- TODO: FOCUS CARDS SHOULD BE CARDS...
        pushAll
          [ FocusCards [toCard obligation]
          , ObligationMessage (toId obligation) $ RevealObligation ident
          , UnfocusCards
          , ObligationMessage (toId obligation) $ ResolvedObligation ident
          ]
        pure
          $ g
          & (entitiesL . obligationsL %~ insert (toId obligation) obligation)
      SideSchemeType -> do
        let sideScheme = createSideScheme card
        pushAll
          [ SideSchemeMessage (toId sideScheme) SideSchemePlaceInitialThreat
          , SideSchemeMessage (toId sideScheme) RevealSideScheme
          ]
        pure
          $ g
          & (entitiesL . sideSchemesL %~ insert (toId sideScheme) sideScheme)
      _ -> error "Unhandled"
  EndCheckWindows -> pure g
  IdentityEndedTurn ident -> pure $ g & usedAbilitiesL . ix ident %~ filter
    ((/= PerTurn 1) . abilityLimit . fst)
  EndRound -> pure $ g & usedAbilitiesL . each %~ filter
    ((/= PerRound 1) . abilityLimit . fst)
  GameOver status -> do
    clearQueue
    pure $ g & stateL .~ Finished status
  ReturnToHand target -> case target of
    AllyTarget aid -> do
      for_ (lookup aid $ gameAllies g) $ \ally -> pushAll $ map
        (IdentityMessage (getAllyController ally))
        [ AllyRemoved aid
        , AddToHand $ MkPlayerCard
          (CardId $ unAllyId aid)
          (getCardDef ally)
          (Just $ getAllyController ally)
          (Just $ getAllyController ally)
        ]
      pure $ g & entitiesL . alliesL %~ delete aid
    _ -> error "unhandled"
  FocusCards cards -> pure $ g & focusedCardsL .~ cards
  UnfocusCards -> pure $ g & focusedCardsL .~ []
  CheckWindows windows -> do
    abilities <- getsGame getAbilities
    usedAbilities <- getUsedAbilities
    attrs <- getActivePlayer
    playableCards <- concatMapM
      (\w -> map (, Just w) <$> getWindowPlayableCards w attrs)
      windows
    ident <- toId <$> getActivePlayer
    validAbilities <- filterM
      (andM . sequence
        [ pure . passesUseLimit ident usedAbilities
        , passesCriteria ident
        , passesCanAffordCost ident
        , \a -> anyM (`abilityInWindow` a) windows
        ]
      )
      abilities
    let forcedAbiltiies = filter isForcedAbility validAbilities
    case (forcedAbiltiies, validAbilities, playableCards) of
      ([], [], []) -> pure ()
      ([], as, cs) ->
        push
          $ Ask ident
          . ChooseOne
          $ Label "Use no responses/interrupts" []
          : map (UseAbility . (choicesL <>~ [Run [CheckWindows windows]])) as
          <> map
               (\(c, mwindow) -> TargetLabel
                 (CardIdTarget $ pcCardId c)
                 [PlayCard c mwindow, Run [CheckWindows windows]]
               )
               cs
      (forced, _, _) -> push $ Ask ident . ChooseOne $ map
        (UseAbility . (choicesL <>~ [Run [CheckWindows windows]]))
        forced
    pure g
  DeclareDefense ident enemyId defensePriority -> do
    allies <- selectList UnexhaustedAlly
    identities <- if defensePriority == AllyIfAble && not (null allies)
      then pure mempty
      else select $ UnexhaustedIdentity <> HeroIdentity
    push
      $ Ask ident
      . ChooseOne
      $ Label "No defenders" []
      : [ Defend enemyId | ident `member` identities ]
      <> map (`AllyDefend` enemyId) allies
    pure g
  _ -> pure g

getWindowPlayableCards
  :: (HasCallStack, MonadGame env m)
  => Window
  -> PlayerIdentity
  -> m [PlayerCard]
getWindowPlayableCards window player = filterM
  (isWindowPlayable window player)
  cards
  where cards = unHand $ playerIdentityHand player

isWindowPlayable
  :: (HasCallStack, MonadGame env m)
  => Window
  -> PlayerIdentity
  -> PlayerCard
  -> m Bool
isWindowPlayable window attrs c = do
  resources <- getAvailableResourcesFor (Just c)
  modifiedCost <- getModifiedCost attrs c
  passedCriteria <- checkCriteria (cdCriteria def <> toAdditionalCriteria def)
  passedWindow <- maybe
    (pure False)
    (\matcher -> windowMatches matcher window GameSource)
    (cdResponseWindow def)
  pure $ length resources >= modifiedCost && passedCriteria && passedWindow
 where
  ident = toId attrs
  def = getCardDef c
  checkCriteria = \case
    IsSelf -> error "Irrelevant"
    OwnsThis -> error "Irrelevant"
    NoCriteria -> pure True
    Never -> pure False
    InHeroForm -> member ident <$> select HeroIdentity
    InAlterEgoForm -> member ident <$> select AlterEgoIdentity
    Unexhausted -> member ident <$> select UnexhaustedIdentity
    Exhausted -> member ident <$> select Matchers.ExhaustedIdentity
    SelfMatches identityMatcher ->
      member ident <$> select (IdentityWithId ident <> identityMatcher)
    Criteria xs -> allM checkCriteria xs
    MinionExists m -> selectAny m
    EnemyExists m -> selectAny m
    CharacterExists m -> selectAny m
    SchemeExists m -> selectAny m
    AllyExists m -> selectAny m
    ExtendedCardExists m -> selectAny m

abilityInWindow :: MonadGame env m => Window -> Ability -> m Bool
abilityInWindow window a = maybe
  (pure False)
  (\matcher -> windowMatches matcher window source)
  (abilityWindow a)
  where source = abilitySource a

instance RunMessage Entities where
  runMessage msg entities =
    traverseOf (playersL . each) (runMessage msg) entities
      >>= traverseOf (alliesL . each) (runMessage msg)
      >>= traverseOf (supportsL . each) (runMessage msg)
      >>= traverseOf (upgradesL . each) (runMessage msg)
      >>= traverseOf (treacheriesL . each) (runMessage msg)
      >>= traverseOf (obligationsL . each) (runMessage msg)
      >>= traverseOf (villainsL . each) (runMessage msg)
      >>= traverseOf (eventsL . each) (runMessage msg)
      >>= traverseOf (effectsL . each) (runMessage msg)
      >>= traverseOf (attachmentsL . each) (runMessage msg)
      >>= traverseOf (minionsL . each) (runMessage msg)
      >>= traverseOf (sideSchemesL . each) (runMessage msg)
      >>= traverseOf (mainSchemesL . each) (runMessage msg)

instance RunMessage Game where
  runMessage msg g =
    runPreGameMessage msg g
      >>= traverseOf scenarioL (runMessage msg)
      >>= traverseOf entitiesL (runMessage msg)
      >>= traverseOf boostEntitiesL (runMessage (Boost msg))
      >>= runGameMessage msg

class HasGame a where
  game :: a -> IORef Game

createAlly :: IdentityId -> PlayerCard -> Ally
createAlly ident card =
  lookupAlly (toCardCode card) ident (AllyId $ unCardId $ pcCardId card)

createSupport :: IdentityId -> PlayerCard -> Support
createSupport ident card =
  lookupSupport (toCardCode card) ident (SupportId $ unCardId $ pcCardId card)

createUpgrade :: IdentityId -> PlayerCard -> Upgrade
createUpgrade ident card =
  lookupUpgrade (toCardCode card) ident (UpgradeId $ unCardId $ pcCardId card)

createEvent :: IdentityId -> PlayerCard -> Event
createEvent ident card =
  lookupEvent (toCardCode card) ident (EventId $ unCardId $ pcCardId card)

createSideScheme :: EncounterCard -> SideScheme
createSideScheme card =
  lookupSideScheme (toCardCode card) (SideSchemeId $ unCardId $ ecCardId card)

createTreachery :: EncounterCard -> Treachery
createTreachery card =
  lookupTreachery (toCardCode card) (TreacheryId $ unCardId $ ecCardId card)

createObligation :: EncounterCard -> Obligation
createObligation card =
  lookupObligation (toCardCode card) (ObligationId $ unCardId $ ecCardId card)

createMinion :: IdentityId -> EncounterCard -> Minion
createMinion ident card =
  lookupMinion (toCardCode card) ident (MinionId $ unCardId $ ecCardId card)

createAttachment :: EncounterCard -> Attachment
createAttachment card =
  lookupAttachment (toCardCode card) (AttachmentId $ unCardId $ ecCardId card)

createEffect :: EffectId -> CardCode -> Source -> EntityMatcher -> Effect
createEffect ident cardCode source matcher =
  lookupEffect cardCode source matcher ident

newGame :: PlayerIdentity -> Scenario -> Game
newGame player scenario = Game
  { gamePhase = PlayerPhase
  , gameState = Unstarted
  , gamePlayerOrder = [toId player]
  , gameBoostEntities = defaultEntities
  , gameRemovedEntities = defaultEntities
  , gameEntities = defaultEntities
    { entitiesPlayers = fromList [(toId player, player)]
    }
  , gameActiveCost = Nothing
  , gameWindowDepth = 0
  , gameWindows = []
  , gameQuestion = mempty
  , gameUsedAbilities = mempty
  , gameActivePlayer = toId player
  , gameScenario = scenario
  , gameFocusedCards = []
  , gameBoostCards = mempty
  }

addPlayer :: MonadGame env m => PlayerIdentity -> m ()
addPlayer player =
  withGame_
    $ (entitiesL . playersL %~ insert (toId player) player)
    . (playerOrderL <>~ [toId player])

withGame :: MonadGame env m => (Game -> (Game, a)) -> m a
withGame f = do
  gameRef <- asks game
  atomicModifyIORef' gameRef f

withGame_ :: MonadGame env m => (Game -> Game) -> m ()
withGame_ f = withGame ((, ()) . f)

withGameM :: MonadGame env m => (Game -> m Game) -> m ()
withGameM f = getGame >>= f >>= withGame_ . const

getsGame :: MonadGame env m => (Game -> a) -> m a
getsGame f = withGame (id &&& f)

getUsedAbilities :: MonadGame env m => m (HashMap IdentityId [Ability])
getUsedAbilities = getsGame (HashMap.map (map fst) . view usedAbilitiesL)

class
  ( MonadCatch m
  , MonadThrow m
  , MonadIO m
  , MonadReader env m
  , HasGame env
  , HasQueue env
  , HasDebugLogger env
  , MonadRandom m
  , CoerceRole m
  ) =>
  MonadGame env m
    | env -> m
    , m -> env

initPlayer :: MonadRandom m => CardCode -> Deck -> m PlayerIdentity
initPlayer cardCode deck = do
  ident <- getRandom
  let
    mAlterEgo = do
      def <- lookup (toAlterEgoCardCode cardCode) allAlterEgosMap
      lookupAlterEgo def ident
    mHero = do
      def <- lookup (toHeroCardCode cardCode) allHeroesMap
      lookupHero def ident
  case (mAlterEgo, mHero) of
    (Just alterEgoSide, Just heroSide) ->
      pure . setDeck deck $ createIdentity ident alterEgoSide heroSide
    _ -> error "stuff"

createPlayer :: MonadGame env m => CardCode -> Deck -> m ()
createPlayer cardCode deck = do
  playerIdentity <- initPlayer cardCode deck
  addPlayer playerIdentity

getGame :: MonadGame env m => m Game
getGame = readIORef =<< asks game

instance HasAbilities Game where
  getAbilities g =
    concatMap getAbilities (elems $ gamePlayers g)
      <> concatMap getAbilities (elems $ gameAllies g)
      <> concatMap getAbilities (elems $ gameSupports g)
      <> concatMap getAbilities (elems $ gameUpgrades g)
      <> concatMap getAbilities (elems $ gameVillains g)
      <> concatMap getAbilities (elems $ gameAttachments g)
      <> concatMap getAbilities (elems $ gameMinions g)

runGameMessages :: MonadGame env m => m ()
runGameMessages = do
  mMsg <- pop
  for_ mMsg debug
  for_ mMsg $ \case
    Ask ident choices -> do
      withGame_ $ questionL .~ fromList [(ident, choices)]
    other -> do
      withGame_ $ questionL .~ mempty
      withGameM $ runMessage other
      runGameMessages

-- TODO: implement this for api
replayChoices :: MonadGame env m => [Diff.Patch] -> m ()
replayChoices _ = pure ()

getAvailablePaymentSources :: MonadGame env m => m [PlayerCard]
getAvailablePaymentSources = do
  players <- toList <$> getsGame gamePlayers
  pure $ concatMap (unHand . playerIdentityHand) players

getAvailableResourcesFor
  :: (HasCallStack, MonadGame env m) => Maybe PlayerCard -> m [Resource]
getAvailableResourcesFor mc = do
  players <- toList <$> getsGame gamePlayers
  abilitiesResources <- concatMapM abilityResources =<< getResourceAbilities
  playerResources <- concatMapM (`resourcesFor` mc) players
  pure $ playerResources <> abilitiesResources

getResourceAbilities :: (HasCallStack, MonadGame env m) => m [Ability]
getResourceAbilities = do
  player <- getActivePlayer
  abilities <- getsGame getAbilities
  usedAbilities <- getUsedAbilities
  filterM
    (andM . sequence
      [ pure . passesUseLimit (toId player) usedAbilities
      , pure . (== Resource) . abilityType
      , passesCanAffordCost (toId player)
      , passesCriteria (toId player)
      ]
    )
    abilities

gameSelectIdentity
  :: MonadGame env m => IdentityMatcher -> m (HashSet IdentityId)
gameSelectIdentity m = do
  identities <- toList <$> getsGame gamePlayers
  result <- filterM (matchFilter m) identities
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    AnyIdentity -> pure . const True
    HeroIdentity -> pure . isHero
    AlterEgoIdentity -> pure . isAlterEgo
    UnexhaustedIdentity -> pure . not . isExhausted
    Matchers.ExhaustedIdentity -> pure . isExhausted
    ConfusedIdentity -> pure . identityIsConfused
    StunnedIdentity -> pure . identityIsStunned
    IdentityEngagedWith minionMatcher -> \ident -> do
      minions <- select minionMatcher
      pure
        . not
        . null
        $ playerIdentityMinions ident
        `HashSet.intersection` minions
    IdentityWithId ident' -> pure . (== ident') . toId
    IdentityWithTrait trait -> fmap (member trait) . getTraits
    IdentityWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . identityDamage
    IdentityMatchAll xs -> andM . traverse matchFilter xs
    FirstPlayer -> \a -> do
      ps <- getPlayers
      pure $ case ps of
        fp : _ -> toId a == fp
        _ -> False
    You -> \a -> do
      ap <- getActivePlayer
      pure $ a == ap

gameSelectCharacter
  :: MonadGame env m => CharacterMatcher -> m (HashSet CharacterId)
gameSelectCharacter = \case
  CharacterWithDamage gameValueMatcher -> do
    villains <- map VillainCharacter
      <$> selectList (VillainWithDamage gameValueMatcher)
    minions <- map MinionCharacter
      <$> selectList (MinionWithDamage gameValueMatcher)
    identities <- map IdentityCharacter
      <$> selectList (IdentityWithDamage gameValueMatcher)
    allies <- map AllyCharacter <$> selectList (AllyWithDamage gameValueMatcher)
    pure . HashSet.fromList $ villains <> minions <> identities <> allies
  DamageableCharacter -> do
    let
      enemyToCharacter = \case
        EnemyMinionId mid -> MinionCharacter mid
        EnemyVillainId vid -> VillainCharacter vid
    enemies <- selectMap enemyToCharacter DamageableEnemy
    identities <- selectMap IdentityCharacter AnyIdentity
    allies <- selectMap AllyCharacter AnyAlly
    pure . HashSet.fromList $ enemies <> identities <> allies
  AnyCharacter -> do
    villains <- selectMap VillainCharacter AnyVillain
    minions <- selectMap MinionCharacter AnyMinion
    identities <- selectMap IdentityCharacter AnyIdentity
    allies <- selectMap AllyCharacter AnyAlly
    pure . HashSet.fromList $ villains <> minions <> identities <> allies
  CharacterMatches [] -> pure mempty
  CharacterMatches (x : xs) ->
    foldl' HashSet.intersection
      <$> gameSelectCharacter x
      <*> traverse gameSelectCharacter xs

gameSelectExtendedCard
  :: MonadGame env m => ExtendedCardMatcher -> m (HashSet PlayerCard)
gameSelectExtendedCard m = do
  let excludedCards = getExcludedCards m
  players <- toList <$> getsGame gamePlayers
  oldVal <- getsGame id
  withGameM $ \g -> foldlM
    (\g' -> (`runMessage` g') . RemoveFromGame . CardIdTarget . pcCardId)
    g
    excludedCards
  let
    allCards =
      concatMap (unHand . playerIdentityHand) players
        <> concatMap (unDiscard . playerIdentityDiscard) players
        <> concatMap (unDeck . playerIdentityDeck) players
  result <- go players allCards m
  withGame_ (const oldVal)
  pure $ HashSet.fromList result
 where
  getExcludedCards = \case
    NotCard c -> [c]
    ExtendedCardMatches ms -> concatMap getExcludedCards ms
    _ -> []
  go players cards = \case
    NotCard c -> pure $ filter (/= c) cards
    AffordableCardBy identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      let players' = filter ((`elem` identities) . toId) players
      inner <- go players' cards extendedCardMatcher
      concatMapM (\ident -> filterM (isPlayable ident) inner) players'
    BasicCardMatches cardMatcher -> do
      pure $ filter (cardMatch cardMatcher) cards
    InDiscardOf identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      let
        players' = filter ((`elem` identities) . toId) players
        cards' = filter (`elem` cards)
          $ concatMap (unDiscard . playerIdentityDiscard) players'
      go players cards' extendedCardMatcher
    TopmostCardInDiscardOf identityMatcher cardMatcher -> do
      identities <- selectList identityMatcher
      let players' = filter ((`elem` identities) . toId) players
      pure $ mapMaybe
        (find (and . sequence [(`elem` cards), cardMatch cardMatcher])
        . unDiscard
        . playerIdentityDiscard
        )
        players'
    ExtendedCardMatches matchers -> foldlM (go players) cards matchers

gameSelectEncounterCard
  :: MonadGame env m => EncounterCardMatcher -> m (HashSet EncounterCard)
gameSelectEncounterCard m = case m of
  NemesisSetFor identityId -> do
    mIdentity <- getsGame $ lookup identityId . entitiesPlayers . gameEntities
    case mIdentity of
      Nothing -> pure mempty
      Just iid -> do
        cards <- gatherEncounterSet $ getNemesisSet $ toCardCode iid
        pure $ HashSet.fromList cards

gameSelectAlly :: MonadGame env m => AllyMatcher -> m (HashSet AllyId)
gameSelectAlly m = do
  allies <- toList <$> getsGame gameAllies
  result <- filterM (matchFilter m) allies
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    AnyAlly -> pure . const True
    UnexhaustedAlly -> pure . not . isExhausted
    ExhaustedAlly -> pure . isExhausted
    AllyControlledBy identityMatcher -> \ally -> do
      identities <- select identityMatcher
      pure $ member (getAllyController ally) identities
    AllyWithUses gameValueMatcher ->
      gameValueMatches gameValueMatcher . getAllyUses
    AllyWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . getAllyDamage
    AllyWithId ident' -> pure . (== ident') . toId
    AllyMatches xs -> andM . traverse matchFilter xs

gameSelectSupport :: MonadGame env m => SupportMatcher -> m (HashSet SupportId)
gameSelectSupport = \case
  UnexhaustedSupport -> do
    supports <- toList <$> getsGame gameSupports
    pure $ HashSet.fromList $ map toId $ filter (not . isExhausted) supports
  SupportControlledBy identityMatcher -> do
    supports <- toList <$> getsGame gameSupports
    identities <- select identityMatcher
    pure $ HashSet.fromList $ map toId $ filter
      ((`member` identities) . getSupportController)
      supports
  SupportWithUses gameValueMatcher -> do
    upgrades <- toList <$> getsGame gameSupports
    HashSet.fromList
      . map toId
      <$> filterM (gameValueMatches gameValueMatcher . getSupportUses) upgrades

gameSelectUpgrade :: MonadGame env m => UpgradeMatcher -> m (HashSet UpgradeId)
gameSelectUpgrade m = do
  upgrades <- toList <$> getsGame gameUpgrades
  result <- filterM (matchFilter m) upgrades
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    UnexhaustedUpgrade -> pure . not . isExhausted
    UpgradeWithUses gameValueMatcher ->
      gameValueMatches gameValueMatcher . getUpgradeUses
    UpgradeWithTrait t -> pure . member t . cdTraits . getCardDef
    UpgradeControlledBy identityMatcher -> \upgrade -> do
      identities <- select identityMatcher
      pure $ getUpgradeController upgrade `member` identities
    UpgradeMatches xs -> andM . traverse matchFilter xs

gameSelectAttachment
  :: MonadGame env m => AttachmentMatcher -> m (HashSet AttachmentId)
gameSelectAttachment m = do
  attachments <- toList <$> getsGame gameAttachments
  result <- filterM (matchFilter m) attachments
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    AttachmentWithId ident' -> pure . (== ident') . toId

gameSelectEnemy :: MonadGame env m => EnemyMatcher -> m (HashSet EnemyId)
gameSelectEnemy m = do
  villains <- toList <$> getsGame gameVillains
  minions <- toList <$> getsGame gameMinions
  removedVillains <- toList <$> getsGame (view (removedEntitiesL . villainsL))
  removedMinions <- toList <$> getsGame (view (removedEntitiesL . minionsL))
  villains' <- map (EnemyVillainId . toId)
    <$> filterM (`goVillain` m) (villains <> removedVillains)
  minions' <- map (EnemyMinionId . toId)
    <$> filterM (`goMinion` m) (minions <> removedMinions)
  pure $ HashSet.fromList (villains' <> minions')
 where
  goVillain e = \case
    EnemyWithId enemyId -> pure $ case enemyId of
      EnemyVillainId villainId -> toId e == villainId
      EnemyMinionId _ -> False
    AnyEnemy -> pure True
    VillainEnemy -> pure True
    MinionEnemy -> pure False
    DamageableEnemy -> pure True
    AttackableEnemy -> not <$> selectAny (MinionWithKeyword Guard)
    UndefendedEnemy ->
      pure $ maybe False (not . attackDefended) (villainAttackDetails e)
    NotEnemy m' -> not <$> goVillain e m'
    EnemyIs def -> pure $ def == getCardDef e
    EnemyMatchesAll xs -> allM (goVillain e) xs
  goMinion e = \case
    EnemyWithId enemyId -> pure $ case enemyId of
      EnemyVillainId _ -> False
      EnemyMinionId minionId -> toId e == minionId
    AnyEnemy -> pure True
    VillainEnemy -> pure False
    MinionEnemy -> pure True
    DamageableEnemy -> pure True
    AttackableEnemy -> pure True
    UndefendedEnemy ->
      pure $ maybe False (not . attackDefended) (minionAttackDetails e)
    NotEnemy m' -> not <$> goMinion e m'
    EnemyIs def -> pure $ def == getCardDef e
    EnemyMatchesAll xs -> allM (goMinion e) xs

gameSelectVillain :: MonadGame env m => VillainMatcher -> m (HashSet VillainId)
gameSelectVillain m = do
  villains <- toList <$> getsGame gameVillains
  removedVillains <- toList <$> getsGame (view (removedEntitiesL . villainsL))
  result <- filterM (matchFilter m) (villains <> removedVillains)
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    ActiveVillain -> pure . const True
    AnyVillain -> pure . const True
    VillainWithId ident' -> pure . (== ident') . toId
    VillainWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . villainDamage
    VillainWithToughStatus -> pure . villainIsTough
    VillainMatches xs -> andM . traverse matchFilter xs

gameSelectMinion :: MonadGame env m => MinionMatcher -> m (HashSet MinionId)
gameSelectMinion m = do
  minions <- toList <$> getsGame gameMinions
  removedMinions <- toList <$> getsGame (view (removedEntitiesL . minionsL))
  result <- filterM (matchFilter m) (minions <> removedMinions)
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    AnyMinion -> pure . const True
    MinionWithId ident' -> pure . (== ident') . toId
    MinionWithTrait trait -> fmap (member trait) . getTraits
    MinionWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . getMinionDamage
    MinionEngagedWith identityMatcher -> \minion -> do
      identities <- select identityMatcher
      pure $ getMinionEngagedIdentity minion `member` identities
    MinionIs cardDef -> pure . (== cardDef) . getCardDef
    MinionWithHighestPrintedHitPoints -> \minion -> do
      minions <- toList <$> getsGame gameMinions
      let highest = maximum $ map (unHp . getMinionPrintedHitPoints) minions
      pure . (== highest) . unHp $ getMinionPrintedHitPoints minion
    MinionWithKeyword k -> pure . member k . cdKeywords . getCardDef

gameSelectTreachery
  :: MonadGame env m => TreacheryMatcher -> m (HashSet TreacheryId)
gameSelectTreachery = \case
  AnyTreachery -> do
    treacheries <- toList <$> getsGame gameTreacheries
    pure $ HashSet.fromList $ map toId treacheries

gameSelectScheme :: MonadGame env m => SchemeMatcher -> m (HashSet SchemeId)
gameSelectScheme = \case
  AnyScheme -> do
    mainSchemes <- toList <$> getsGame gameMainSchemes
    sideSchemes <- toList <$> getsGame gameSideSchemes
    pure
      $ HashSet.fromList
      $ map (SchemeMainSchemeId . toId) mainSchemes
      <> map (SchemeSideSchemeId . toId) sideSchemes
  Matchers.MainScheme -> do
    mainSchemeIds <-
      map (SchemeMainSchemeId . toId) . toList <$> getsGame gameMainSchemes
    pure $ HashSet.fromList mainSchemeIds
  SchemeWithId enemyId -> case enemyId of
    SchemeMainSchemeId mainSchemeId -> do
      mainSchemes <- toList <$> getsGame gameMainSchemes
      pure . fromList $ map (SchemeMainSchemeId . toId) $ filter
        ((== mainSchemeId) . toId)
        mainSchemes
    SchemeSideSchemeId sideSchemeId -> HashSet.map SchemeSideSchemeId
      <$> gameSelectSideScheme (SideSchemeWithId sideSchemeId)
  ThwartableScheme -> do
    crisisSideSchemes <- selectAny CrisisSideScheme
    sideSchemes <- toList <$> getsGame gameSideSchemes
    if crisisSideSchemes
      then pure $ HashSet.fromList $ map
        (SchemeSideSchemeId . toId)
        (filter ((> 0) . getSideSchemeThreat) sideSchemes)
      else do
        mainSchemes <- toList <$> getsGame gameMainSchemes
        pure
          $ HashSet.fromList
          $ map
              (SchemeMainSchemeId . toId)
              (filter ((> 0) . getMainSchemeThreat) mainSchemes)
          <> map
               (SchemeSideSchemeId . toId)
               (filter ((> 0) . getSideSchemeThreat) sideSchemes)

gameSelectCountScheme
  :: MonadGame env m => QueryCount SchemeMatcher -> SchemeMatcher -> m Natural
gameSelectCountScheme aggregate matcher = do
  schemes <- toList <$> gameSelectScheme matcher
  case aggregate of
    SchemeThreat -> do
      let
        toThreat = \case
          SchemeMainSchemeId sid -> do
            mMainScheme <- getsGame (lookup sid . gameMainSchemes)
            pure $ maybe 0 getMainSchemeThreat mMainScheme
          SchemeSideSchemeId sid -> do
            mSideScheme <- getsGame (lookup sid . gameSideSchemes)
            pure $ maybe 0 getSideSchemeThreat mSideScheme
      sum <$> traverse toThreat schemes

gameSelectSideScheme
  :: MonadGame env m => SideSchemeMatcher -> m (HashSet SideSchemeId)
gameSelectSideScheme m = do
  sideSchemes <- toList <$> getsGame gameSideSchemes
  result <- filterM (matchFilter m) sideSchemes
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter = \case
    AnySideScheme -> pure . const True
    CrisisSideScheme -> pure . isCrisis
    SideSchemeIs def -> pure . (== def) . getCardDef
    SideSchemeWithId ident -> pure . (== ident) . toId

getModifiers :: (MonadGame env m, IsSource a, IsTarget a) => a -> m [Modifier]
getModifiers a = do
  effects <- toList <$> getsGame gameEffects
  upgrades <- toList <$> getsGame gameUpgrades
  supports <- toList <$> getsGame gameSupports
  attachments <- toList <$> getsGame gameAttachments
  allies <- toList <$> getsGame gameAllies
  minions <- toList <$> getsGame gameMinions
  events <- toList <$> getsGame gameEvents
  players <- toList <$> getsGame gamePlayers
  sideSchemes <- toList <$> getsGame gameSideSchemes
  mconcat <$> sequence
    [ concatMapM (getModifiersFor (toSource a) (toTarget a)) effects
    , concatMapM (getModifiersFor (toSource a) (toTarget a)) upgrades
    , concatMapM (getModifiersFor (toSource a) (toTarget a)) supports
    , concatMapM (getModifiersFor (toSource a) (toTarget a)) attachments
    , concatMapM (getModifiersFor (toSource a) (toTarget a)) allies
    , concatMapM (getModifiersFor (toSource a) (toTarget a)) minions
    , concatMapM (getModifiersFor (toSource a) (toTarget a)) events
    , concatMapM (getModifiersFor (toSource a) (toTarget a)) players
    , concatMapM (getModifiersFor (toSource a) (toTarget a)) sideSchemes
    ]

getCurrentWindows :: MonadGame env m => m [Window]
getCurrentWindows = do
  windows <- getsGame gameWindows
  pure $ case windows of
    [] -> []
    x : _ -> x

getDifficulty :: MonadGame env m => m Difficulty
getDifficulty = getsGame $ getScenarioDifficulty . gameScenario

abilityResources :: MonadGame env m => Ability -> m [Resource]
abilityResources a = go (abilityChoices a)
 where
  go [] = pure []
  go (Pay (ResourcePayment r) : xs) = (r :) <$> go xs
  go (Pay (ResourcePaymentFromCard matcher) : xs) = do
    cards <- selectList matcher
    case cards of
      [] -> go xs
      [c] -> (<>) (printedResources $ getCardDef c) <$> go xs
      _ -> error "invalid matcher"
  go (_ : xs) = go xs

getHazardCount :: MonadGame env m => m Natural
getHazardCount = do
  sideSchemes <- getsGame gameSideSchemes
  pure $ foldr ((+) . cdHazards . getCardDef) 0 sideSchemes

getAccelerationCount :: MonadGame env m => m Natural
getAccelerationCount = do
  tokens <- getsGame (scenarioAccelerationTokens . gameScenario)
  sideSchemes <- getsGame gameSideSchemes
  pure $ foldr ((+) . cdAcceleration . getCardDef) tokens sideSchemes

class Count a where
  data QueryCount a
  selectCount :: MonadGame env m => QueryCount a -> a -> m Natural

class Entity a => GameEntity a where
  getAttrs :: MonadGame env m => EntityId a -> m (EntityAttrs a)

gameSelectCountIdentity
  :: MonadGame env m
  => QueryCount IdentityMatcher
  -> IdentityMatcher
  -> m Natural
gameSelectCountIdentity aggregate matcher = do
  identities <-
    filterM (identityMatches matcher . toId) . toList =<< getsGame gamePlayers
  case aggregate of
    SustainedDamage -> pure . sum $ map identityDamage identities
    HeroAttackDamage -> sum <$> traverse getIdentityHeroAttackDamage identities
