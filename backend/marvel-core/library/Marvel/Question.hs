module Marvel.Question where

import Marvel.Prelude

import Data.List (partition)
import qualified Data.List as L
import Data.Traversable (for)
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.PlayerCard
import Marvel.Card.Side
import Marvel.Exception
import Marvel.Game.Source
import Marvel.Id
import {-# SOURCE #-} Marvel.Message
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target

data Cost = Costs [Cost] | ResourceCost (Maybe Resource) | NoCost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Monoid Cost where
  mempty = NoCost

instance Semigroup Cost where
  NoCost <> x = x
  x <> NoCost = x
  Costs xs <> Costs ys = Costs $ xs <> ys
  Costs xs <> y = Costs $ xs <> [y]
  x <> Costs ys = Costs $ x : ys
  x <> y = Costs [x, y]

costResources :: Cost -> [Maybe Resource]
costResources NoCost = []
costResources (ResourceCost r) = [r]
costResources (Costs ps) = concatMap costResources ps

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

data Choice
  = CardLabel CardCode Choice
  | Label Text [Choice]
  | TargetLabel Target [Choice]
  | EndTurn
  | UseAbility Ability
  | RunAbility Target Natural
  | ChangeForm
  | ChangeToForm Side
  | PlayCard PlayerCard
  | PayWithCard PlayerCard
  | FinishPayment
  | Pay Payment
  | Run [Message]
  | Damage Target Source Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

choiceMessages :: IdentityId -> Choice -> [Message]
choiceMessages ident = \case
  Run msgs -> msgs
  Label _ choices -> concatMap (choiceMessages ident) choices
  TargetLabel _ choices -> concatMap (choiceMessages ident) choices
  CardLabel _ choice -> choiceMessages ident choice
  EndTurn -> [IdentityMessage ident EndedTurn]
  UseAbility a ->
    UsedAbility ident a : concatMap (choiceMessages ident) (abilityChoices a)
  RunAbility target n -> [RanAbility target n]
  ChangeForm -> [IdentityMessage ident ChooseOtherForm]
  ChangeToForm x -> [IdentityMessage ident $ ChangedToForm x]
  PlayCard x -> [IdentityMessage ident $ PlayedCard x]
  PayWithCard c -> [IdentityMessage ident $ PayedWithCard c]
  FinishPayment -> [FinishedPayment]
  Pay payment -> [Paid payment]
  Damage target source n -> case target of
    VillainTarget vid -> [VillainMessage vid $ VillainDamaged source n]
    _ -> error "can not damage target"

chooseOne :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOne ident msgs = push (Ask ident $ ChooseOne msgs)

chooseOrRunOne :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOrRunOne ident = \case
  [] -> throwM NoChoices
  [choice] -> pushAll $ choiceMessages ident choice
  choices -> push (Ask ident $ ChooseOne choices)

choosePlayerOrder :: MonadGame env m => IdentityId -> [IdentityId] -> m ()
choosePlayerOrder ident xs =
  push (Ask ident $ ChoosePlayerOrder (Unsorted xs) mempty)

cardLabel :: HasCardCode a => a -> Choice -> Choice
cardLabel a = CardLabel (toCardCode a)
