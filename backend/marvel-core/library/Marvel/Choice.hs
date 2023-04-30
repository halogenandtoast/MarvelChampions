module Marvel.Choice where

import Marvel.Prelude

import Marvel.Ability.Types hiding (Attack, Thwart)
import Marvel.Damage
import Marvel.Message
import Marvel.Question
import Marvel.ActiveCost.Types
import Marvel.Exception
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window (Window(..), WindowTiming(..))
import Marvel.Window qualified as W

pushChoice
  :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => IdentityId -> Choice -> m ()
pushChoice ident choice = do
  msgs <- choiceMessages ident choice
  pushAll msgs

choiceMessages
  :: (HasQueue m, HasGame m, MonadThrow m, MonadRandom m)
  => IdentityId
  -> Choice
  -> m [Message]
choiceMessages ident = \case
  Run msgs -> pure msgs
  Label _ choices -> concatMapM (choiceMessages ident) choices
  TargetLabel _ choices -> concatMapM (choiceMessages ident) choices
  CardLabel _ choice -> choiceMessages ident choice
  EndTurn -> pure [IdentityMessage ident EndedTurn]
  CreateEffect def source targetChoice -> case targetChoice of
    ChooseAPlayer -> do
      targets <- getPlayers
      let f = CreatedEffect def source . IdentityEntity . IdentityWithId
      case targets of
        [] -> throwM NoChoices
        [x] -> pure [f x]
        xs -> pure
          [ Ask ident $ ChooseOne
              [ TargetLabel (IdentityTarget x) [Run [f x]] | x <- xs ]
          ]
    TargetMatches entityMatcher -> case entityMatcher of
      IdentityEntity matcher -> do
        targets <- selectList matcher
        let f = CreatedEffect def source . IdentityEntity . IdentityWithId
        case targets of
          [] -> throwM NoChoices
          [x] -> pure [f x]
          xs -> pure
            [ Ask ident $ ChooseOne
                [ TargetLabel (IdentityTarget x) [Run [f x]] | x <- xs ]
            ]
      AllyEntity matcher -> do
        targets <- selectList matcher
        let f = CreatedEffect def source . AllyEntity . AllyWithId
        case targets of
          [] -> throwM NoChoices
          [x] -> pure [f x]
          xs -> pure
            [ Ask ident
                $ ChooseOne [ TargetLabel (AllyTarget x) [Run [f x]] | x <- xs ]
            ]
      AttachmentEntity matcher -> do
        targets <- selectList matcher
        let f = CreatedEffect def source . AttachmentEntity . AttachmentWithId
        case targets of
          [] -> throwM NoChoices
          [x] -> pure [f x]
          xs -> pure
            [ Ask ident $ ChooseOne
                [ TargetLabel (AttachmentTarget x) [Run [f x]] | x <- xs ]
            ]
  UseAbility a -> do
    activeCostId <- getRandom
    pure
      [ SetActiveCost $ ActiveCost
          activeCostId
          ident
          (ForAbility a)
          (abilityCost a)
          NoPayment
          Nothing
          mempty
      ]
  -- UseAbility a -> do
  --   rest <- concatMapM (choiceMessages ident) (abilityChoices a)
  --   costs <- costMessages ident a
  --   pure $ UsedAbility ident a : costs <> rest
  RunAbility target n -> do
    windows <- getCurrentWindows
    payment <- getCurrentPayment
    pure [RanAbility target n (map windowType windows) payment, ClearRemoved]
  ChangeForm -> pure [IdentityMessage ident ChooseOtherForm]
  ChangeToForm x -> pure [IdentityMessage ident $ ChangedToForm x]
  PlayCard x mWindow -> pure [IdentityMessage ident $ PlayedCard x mWindow]
  PayWithCard c -> pure [IdentityMessage ident $ PaidWithCard c]
  FinishPayment -> do
    mcost <- getActiveCost
    case mcost of
      Just activeCostId -> pure [FinishedPayment activeCostId]
      Nothing -> error "no active cost"
  ReadyIdentity -> pure [IdentityMessage ident ReadiedIdentity]
  Pay payment -> pure [Paid payment]
  DamageAllEnemies matcher source damage -> do
    enemies <- selectList $ DamageableEnemy <> matcher
    concatMapM
      (\e -> choiceMessages ident (DamageEnemy (EnemyTarget e) source damage))
      enemies
  DamageAllCharacters matcher source damage -> do
    characters <- selectList $ DamageableCharacter <> matcher
    concatMapM
      (\c -> choiceMessages ident (DamageCharacter c source damage))
      characters
  DamageEnemy target source damage -> do
    let
      isIdentity = case source of
        IdentitySource _ -> True
        _ -> False
    case target of
      VillainTarget vid ->
        pure
          $ [ CheckWindows [Window When $ W.DamagedVillain vid damage]
            , VillainMessage vid $ VillainDamaged source damage
            ]
          <> [ CheckWindows
                 [Window After $ W.IdentityAttack ident (EnemyVillainId vid)]
             | damageSource damage == FromAttack && isIdentity
             ]
      MinionTarget mid ->
        pure [MinionMessage mid $ MinionDamaged source damage]
      EnemyTarget enemy -> case enemy of
        EnemyVillainId vid ->
          pure
            $ [ CheckWindows [Window When $ W.DamagedVillain vid damage]
              , VillainMessage vid $ VillainDamaged source damage
              ]
            <> [ CheckWindows [Window After $ W.IdentityAttack ident enemy]
               | damageSource damage == FromAttack && isIdentity
               ]
        EnemyMinionId mid ->
          pure [MinionMessage mid $ MinionDamaged source damage]
      _ -> error "can not damage target"
  ThwartScheme target source n -> case target of
    MainSchemeTarget mid ->
      pure [MainSchemeMessage mid $ MainSchemeThwarted source n]
    SideSchemeTarget sid ->
      pure [SideSchemeMessage sid $ SideSchemeThwarted source n]
    SchemeTarget (SchemeMainSchemeId mid) ->
      pure [MainSchemeMessage mid $ MainSchemeThwarted source n]
    SchemeTarget (SchemeSideSchemeId sid) ->
      pure [SideSchemeMessage sid $ SideSchemeThwarted source n]
    _ -> error $ "can not thwart target: " <> show target
  RemoveThreat source n schemeMatcher -> do
    schemes <- selectList schemeMatcher
    let f target = ThwartScheme target source n
    case schemes of
      [] -> pure []
      [x] -> choiceMessages ident (f $ SchemeTarget x)
      xs -> pure
        [ Ask ident $ ChooseOne
            [ TargetLabel target [f target]
            | x <- xs
            , let target = SchemeTarget x
            ]
        ]
  PlaceThreat _source n schemeMatcher -> do
    schemes <- selectList schemeMatcher
    let
      toMsg (SchemeMainSchemeId msid) =
        MainSchemeMessage msid (MainSchemePlaceThreat n)
      toMsg (SchemeSideSchemeId ssid) =
        SideSchemeMessage ssid (SideSchemePlaceThreat n)
    pure $ map toMsg schemes
  ChooseHeal n characterMatcher -> do
    characters <- selectList characterMatcher
    let f character = Heal character n
    case characters of
      [] -> pure []
      [x] -> choiceMessages ident (f x)
      xs ->
        pure
          [ Ask ident
              $ ChooseOne [ TargetLabel (CharacterTarget x) [f x] | x <- xs ]
          ]
  ChooseDamage source damage enemyMatcher -> do
    enemies <- selectList (enemyMatcher <> DamageableEnemy)
    let f target = DamageEnemy target source damage
    case enemies of
      [] -> pure []
      [x] -> do
        msgs <- choiceMessages ident (f $ EnemyTarget x)
        pure $ msgs <> [ClearRemoved]
      xs -> pure
        [ Ask ident $ ChooseOne
          [ TargetLabel target [f target]
          | x <- xs
          , let target = EnemyTarget x
          ]
        , ClearRemoved
        ]
  Stun target source -> case target of
    VillainTarget vid -> pure [VillainMessage vid $ VillainStunned source]
    MinionTarget mid -> pure [MinionMessage mid $ MinionStunned source]
    EnemyTarget eid -> case eid of
      EnemyVillainId vid -> pure [VillainMessage vid $ VillainStunned source]
      EnemyMinionId mid -> pure [MinionMessage mid $ MinionStunned source]
    _ -> error "can not stun target"
  Confuse target source -> case target of
    VillainTarget vid -> pure [VillainMessage vid $ VillainConfused source]
    MinionTarget vid -> pure [MinionMessage vid $ MinionConfused source]
    _ -> error "can not damage target"
  Recover -> pure [IdentityMessage ident $ SideMessage Recovered]
  Heal characterId n -> case characterId of
    IdentityCharacter ident' ->
      pure [IdentityMessage ident' $ IdentityHealed n]
    AllyCharacter ident' -> pure [AllyMessage ident' $ AllyHealed n]
    VillainCharacter ident' -> pure [VillainMessage ident' $ VillainHealed n]
    MinionCharacter ident' -> pure [MinionMessage ident' $ MinionHealed n]
  DamageCharacter characterId source damage -> case characterId of
    IdentityCharacter ident' ->
      pure [IdentityMessage ident' $ IdentityDamaged source damage]
    AllyCharacter ident' ->
      pure [AllyMessage ident' $ AllyDamaged source damage]
    VillainCharacter ident' ->
      pure [VillainMessage ident' $ VillainDamaged source damage]
    MinionCharacter ident' ->
      pure [MinionMessage ident' $ MinionDamaged source damage]
  Attack -> pure [IdentityMessage ident $ SideMessage Attacked]
  Thwart -> pure [IdentityMessage ident $ SideMessage Thwarted]
  Defend enemyId ->
    pure [IdentityMessage ident $ SideMessage $ Defended enemyId]
  AllyAttack allyId -> pure [AllyMessage allyId AllyAttacked]
  AllyThwart allyId -> pure [AllyMessage allyId AllyThwarted]
  AllyDefend allyId enemyId -> pure [AllyMessage allyId $ AllyDefended enemyId]
  DiscardTarget target -> pure [RemoveFromPlay target]
  DiscardCard card -> pure [DiscardedCard card]
  ChooseEnemy matcher target -> do
    enemies <- selectList matcher
    pure
      [ Ask ident $ ChooseOne
          [ TargetLabel (EnemyTarget e) [Run [ChoseEnemy e target]]
          | e <- enemies
          ]
      ]
  ChooseUpgrade matcher target -> do
    upgrades <- selectList matcher
    pure
      [ Ask ident $ ChooseOne
          [ TargetLabel (UpgradeTarget u) [Run [ChoseUpgrade u target]]
          | u <- upgrades
          ]
      ]
  ChoosePlayer matcher target -> do
    players <- selectList matcher
    pure
      [ Ask ident $ ChooseOne
          [ TargetLabel (IdentityTarget p) [Run [ChosePlayer p target]]
          | p <- players
          ]
      ]
  ChooseDrawCards n identityMatcher -> do
    identities <- selectList identityMatcher
    let f iid = Run [IdentityMessage iid (DrawCards FromDeck n)]
    case identities of
      [] -> pure []
      [x] -> choiceMessages ident (f x)
      xs -> pure
        [ Ask ident
            $ ChooseOne
                [ TargetLabel target [f x]
                | x <- xs
                , let target = IdentityTarget x
                ]
        ]
  ReturnTargetToHand target -> pure [ReturnToHand target]
  ChooseOneLabelChoice choicePairs ->
    pure [Ask ident $ ChooseOne $ map (\(t, c) -> Label t [c]) choicePairs]

chooseOne :: HasQueue m => IdentityId -> [Choice] -> m ()
chooseOne ident msgs = push (Ask ident $ ChooseOne msgs)

chooseOneAtATime :: HasQueue m => IdentityId -> [Choice] -> m ()
chooseOneAtATime ident msgs = push (Ask ident $ ChooseOneAtATime msgs)

chooseOrRunOne
  :: (HasQueue m, HasGame m, MonadThrow m, MonadRandom m) => IdentityId -> [Choice] -> m ()
chooseOrRunOne ident = \case
  [] -> throwM NoChoices
  [choice] -> pushAll =<< choiceMessages ident choice
  choices -> push (Ask ident $ ChooseOne choices)

choosePlayerOrder :: HasQueue m => IdentityId -> [IdentityId] -> m ()
choosePlayerOrder ident xs =
  push (Ask ident $ ChoosePlayerOrder (Unsorted xs) mempty)

