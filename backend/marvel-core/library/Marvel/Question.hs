module Marvel.Question where

import Marvel.Prelude

import Data.List (partition)
import Data.List qualified as L
import Marvel.Ability hiding (Attack, Thwart)
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.PlayerCard
import Marvel.Card.Side
import Marvel.Cost
import Marvel.Exception
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly)
import {-# SOURCE #-} Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target
import Marvel.Window (Window(..), WindowTiming(..))
import Marvel.Window qualified as W

data Payment = Payments [Payment] | ResourcePayment Resource | NoPayment
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

paymentResources :: Payment -> [Resource]
paymentResources NoPayment = []
paymentResources (ResourcePayment r) = [r]
paymentResources (Payments ps) = concatMap paymentResources ps

instance Semigroup Payment where
  NoPayment <> x = x
  x <> NoPayment = x
  Payments xs <> Payments ys = Payments $ xs <> ys
  x <> Payments ys = Payments $ x : ys
  Payments xs <> y = Payments $ xs <> [y]
  x <> y = Payments [x, y]

instance Monoid Payment where
  mempty = NoPayment

data ActiveCost = ActiveCost
  { activeCostIdentityId :: IdentityId
  , activeCostTarget :: ActiveCostTarget
  , activeCostCost :: Cost
  , activeCostPayment :: Payment
  , activeCostWindow :: Maybe Window
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

resourceCostPaid :: ActiveCost -> Bool
resourceCostPaid ActiveCost {..} =
  let
    (rs, mrs) =
      first catMaybes $ partition isJust (costResources activeCostCost)
    prs = paymentResources activeCostPayment
  in flip evalState prs $ do
    l <- fmap and $ for rs $ \r -> do
      prs' <- get
      case (r `elem` prs', Wild `elem` prs') of
        (False, False) -> pure False
        (True, _) -> do
          put $ L.delete r prs'
          pure True
        (_, True) -> do
          put $ L.delete Wild prs'
          pure True
    prs' <- get
    pure $ l && length prs' >= length mrs

data ActiveCostTarget = ForCard PlayerCard | ForAbility Ability
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Question
  = ChooseOne [Choice]
  | ChooseOneAtATime [Choice]
  | ChoosePlayerOrder (Unsorted IdentityId) (Sorted IdentityId)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Sorted a = Sorted { unSorted :: [a] }
  deriving newtype (Show, Semigroup, Monoid, Eq, ToJSON, FromJSON)

newtype Unsorted a = Unsorted { unUnsorted :: [a] }
  deriving newtype (Show, Semigroup, Monoid, Eq, ToJSON, FromJSON)

data ChooseATarget = ChooseAPlayer | TargetMatches EntityMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Choice
  = CardLabel CardCode Choice
  | Label Text [Choice]
  | TargetLabel Target [Choice]
  | EndTurn
  | UseAbility Ability
  | RunAbility Target Natural
  | ChangeForm
  | ChangeToForm Side
  | PlayCard PlayerCard (Maybe Window)
  | PayWithCard PlayerCard
  | FinishPayment
  | Pay Payment
  | Run [Message]
  | DamageEnemy Target Source Natural
  | ThwartScheme Target Source Natural
  | Stun Target Source
  | Confuse Target Source
  | Recover
  | Heal CharacterId Natural
  | DamageCharacter CharacterId Source Natural
  | Attack
  | Thwart
  | Defend EnemyId
  | AllyAttack AllyId
  | AllyThwart AllyId
  | AllyDefend AllyId EnemyId
  | CreateEffect CardDef Source ChooseATarget
  | RemoveThreat Source Natural SchemeMatcher
  | PlaceThreat Source Natural SchemeMatcher
  | ChooseDamage Source Natural EnemyMatcher
  | DiscardTarget Target
  | YouDrawCards Natural
  | ReturnTargetToHand Target
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

choiceMessages :: MonadGame env m => IdentityId -> Choice -> m [Message]
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
            [ Ask ident
                $ ChooseOne [ TargetLabel (AttachmentTarget x) [Run [f x]] | x <- xs ]
            ]
  UseAbility a -> do
    rest <- concatMapM (choiceMessages ident) (abilityChoices a)
    pure $ UsedAbility ident a : costMessages ident a <> rest
  RunAbility target n -> do
    windows <- getCurrentWindows
    pure [RanAbility target n $ map windowType windows]
  ChangeForm -> pure [IdentityMessage ident ChooseOtherForm]
  ChangeToForm x -> pure [IdentityMessage ident $ ChangedToForm x]
  PlayCard x mWindow -> pure [IdentityMessage ident $ PlayedCard x mWindow]
  PayWithCard c -> pure [IdentityMessage ident $ PaidWithCard c]
  FinishPayment -> pure [FinishedPayment]
  Pay payment -> pure [Paid payment]
  DamageEnemy target source n -> case target of
    VillainTarget vid -> pure
      [ CheckWindows [Window When $ W.DamagedVillain vid n]
      , VillainMessage vid $ VillainDamaged source n
      ]
    MinionTarget mid -> pure [MinionMessage mid $ MinionDamaged source n]
    EnemyTarget enemy -> case enemy of
      EnemyVillainId vid -> pure
        [ CheckWindows [Window When $ W.DamagedVillain vid n]
        , VillainMessage vid $ VillainDamaged source n
        ]
      EnemyMinionId mid -> pure [MinionMessage mid $ MinionDamaged source n]
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
      toMsg (SchemeMainSchemeId msid) = MainSchemeMessage msid (MainSchemePlaceThreat n)
      toMsg (SchemeSideSchemeId ssid) = SideSchemeMessage ssid (SideSchemePlaceThreat n)
    pure $ map toMsg schemes
  ChooseDamage source n enemyMatcher -> do
    enemies <- selectList enemyMatcher
    let f target = DamageEnemy target source n
    case enemies of
      [] -> pure []
      [x] -> choiceMessages ident (f $ EnemyTarget x)
      xs -> pure
        [ Ask ident $ ChooseOne
            [ TargetLabel target [f target]
            | x <- xs
            , let target = EnemyTarget x
            ]
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
  DamageCharacter characterId source n -> case characterId of
    IdentityCharacter ident' ->
      pure [IdentityMessage ident' $ IdentityDamaged source n]
    AllyCharacter ident' -> pure [AllyMessage ident' $ AllyDamaged source n]
    VillainCharacter ident' -> pure [VillainMessage ident' $ VillainDamaged source n]
    MinionCharacter ident' -> pure [MinionMessage ident' $ MinionDamaged source n]
  Attack -> pure [IdentityMessage ident $ SideMessage Attacked]
  Thwart -> pure [IdentityMessage ident $ SideMessage Thwarted]
  Defend enemyId ->
    pure [IdentityMessage ident $ SideMessage $ Defended enemyId]
  AllyAttack allyId -> pure [AllyMessage allyId AllyAttacked]
  AllyThwart allyId -> pure [AllyMessage allyId AllyThwarted]
  AllyDefend allyId enemyId -> pure [AllyMessage allyId $ AllyDefended enemyId]
  DiscardTarget target -> pure [RemoveFromPlay target]
  YouDrawCards n -> pure [IdentityMessage ident $ DrawCards FromDeck n]
  ReturnTargetToHand target -> pure [ReturnToHand target]

costMessages :: IdentityId -> Ability -> [Message]
costMessages iid a = go (abilityCost a)
 where
  go = \case
    NoCost -> []
    DamageCost n -> [IdentityMessage iid $ IdentityDamaged (abilitySource a) n]
    ExhaustCost -> case abilitySource a of
      IdentitySource ident -> [IdentityMessage ident ExhaustedIdentity]
      AllySource ident -> [AllyMessage ident ExhaustedAlly]
      SupportSource ident -> [SupportMessage ident ExhaustedSupport]
      UpgradeSource ident -> [UpgradeMessage ident ExhaustedUpgrade]
      _ -> error "Unhandled"
    UseCost -> case abilitySource a of
      UpgradeSource ident -> [UpgradeMessage ident SpendUpgradeUse]
      AllySource ident -> [AllyMessage ident SpendAllyUse]
      _ -> error "Unhandled"
    ResourceCost mr -> do
      [SetActiveCost $ ActiveCost iid (ForAbility a) (ResourceCost mr) NoPayment Nothing]
    MultiResourceCost rs -> do
      [SetActiveCost $ ActiveCost iid (ForAbility a) (MultiResourceCost rs) NoPayment Nothing]
    Costs xs -> concatMap go xs

chooseOne :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOne ident msgs = push (Ask ident $ ChooseOne msgs)

chooseOneAtATime :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOneAtATime ident msgs = push (Ask ident $ ChooseOneAtATime msgs)

chooseOrRunOne :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOrRunOne ident = \case
  [] -> throwM NoChoices
  [choice] -> pushAll =<< choiceMessages ident choice
  choices -> push (Ask ident $ ChooseOne choices)

choosePlayerOrder :: MonadGame env m => IdentityId -> [IdentityId] -> m ()
choosePlayerOrder ident xs =
  push (Ask ident $ ChoosePlayerOrder (Unsorted xs) mempty)

cardLabel :: HasCardCode a => a -> Choice -> Choice
cardLabel a = CardLabel (toCardCode a)
