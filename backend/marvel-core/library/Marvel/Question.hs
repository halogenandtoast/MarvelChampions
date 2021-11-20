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
import Marvel.Window

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

newtype ActiveCostTarget = ForCard PlayerCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Question
  = ChooseOne [Choice]
  | ChoosePlayerOrder (Unsorted IdentityId) (Sorted IdentityId)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Sorted a = Sorted { unSorted :: [a] }
  deriving newtype (Show, Semigroup, Monoid, Eq, ToJSON, FromJSON)

newtype Unsorted a = Unsorted { unUnsorted :: [a] }
  deriving newtype (Show, Semigroup, Monoid, Eq, ToJSON, FromJSON)

data ChooseATarget = ChooseAPlayer
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
  | Heal Natural
  | Attack
  | Thwart
  | Defend EnemyId
  | AllyAttack AllyId
  | AllyThwart AllyId
  | AllyDefend AllyId EnemyId
  | CreateEffect CardDef Source ChooseATarget
  | RemoveThreat Source Natural SchemeMatcher
  | DiscardTarget Target
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
  UseAbility a -> do
    rest <- concatMapM (choiceMessages ident) (abilityChoices a)
    pure $ UsedAbility ident a : costMessages a <> rest
  RunAbility target n -> pure [RanAbility target n]
  ChangeForm -> pure [IdentityMessage ident ChooseOtherForm]
  ChangeToForm x -> pure [IdentityMessage ident $ ChangedToForm x]
  PlayCard x mWindow -> pure [IdentityMessage ident $ PlayedCard x mWindow]
  PayWithCard c -> pure [IdentityMessage ident $ PaidWithCard c]
  FinishPayment -> pure [FinishedPayment]
  Pay payment -> pure [Paid payment]
  DamageEnemy target source n -> case target of
    VillainTarget vid -> pure [VillainMessage vid $ VillainDamaged source n]
    MinionTarget vid -> pure [MinionMessage vid $ MinionDamaged source n]
    _ -> error "can not damage target"
  ThwartScheme target source n -> case target of
    MainSchemeTarget mid ->
      pure [MainSchemeMessage mid $ MainSchemeThwarted source n]
    SchemeTarget (SchemeMainSchemeId mid) ->
      pure [MainSchemeMessage mid $ MainSchemeThwarted source n]
    _ -> error "can not thwart target"
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
  Heal n -> pure [IdentityMessage ident $ IdentityHealed n]
  Attack -> pure [IdentityMessage ident $ SideMessage Attacked]
  Thwart -> pure [IdentityMessage ident $ SideMessage Thwarted]
  Defend enemyId ->
    pure [IdentityMessage ident $ SideMessage $ Defended enemyId]
  AllyAttack allyId -> pure [AllyMessage allyId AllyAttacked]
  AllyThwart allyId -> pure [AllyMessage allyId AllyThwarted]
  AllyDefend allyId enemyId -> pure [AllyMessage allyId $ AllyDefended enemyId]
  DiscardTarget target -> pure [RemoveFromPlay target]

costMessages :: Ability -> [Message]
costMessages a = go (abilityCost a)
 where
  go = \case
    NoCost -> []
    ExhaustCost -> case abilitySource a of
      IdentitySource ident -> [IdentityMessage ident ExhaustedIdentity]
      AllySource ident -> [AllyMessage ident ExhaustedAlly]
      SupportSource ident -> [SupportMessage ident ExhaustedSupport]
      UpgradeSource ident -> [UpgradeMessage ident ExhaustedUpgrade]
      _ -> error "Unhandled"
    UseCost -> case abilitySource a of
      UpgradeSource ident -> [UpgradeMessage ident SpendUpgradeUse]
      _ -> error "Unhandled"
    Costs xs -> concatMap go xs
    _ -> error $ "Unhandled: " <> show (abilityCost a)

chooseOne :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOne ident msgs = push (Ask ident $ ChooseOne msgs)

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
