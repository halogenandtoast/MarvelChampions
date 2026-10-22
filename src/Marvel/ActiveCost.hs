{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.ActiveCost (
  module Marvel.ActiveCost,
  module Marvel.ActiveCost.Types,
) where

import Marvel.Prelude

import Control.Monad.State
import Data.List (partition)
import Data.List qualified as L
import Marvel.Ability (limitedAbility)
import Marvel.Ability.Types
import Marvel.Criteria (Criteria (NoCriteria))
import Marvel.ActiveCost.Types
import Marvel.Card
import Marvel.Choice
import Marvel.Cost
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Message
import Marvel.Payment
import Marvel.Projection
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource
import Marvel.Window.Types (Window (..), WindowTiming (After), WindowType (CardPlayed))

-- | Pay a cost a card names as it resolves, as Lightning Strike's "spend X [energy]":
-- once it is paid, the card is answered with @RanAbility@ for that step and the payment.
payForCard :: (MonadRandom m, HasQueue m, IsRef a) => IdentityId -> a -> Natural -> Cost -> m ()
payForCard ident a step cost = do
  activeCostId <- getRandom
  let ab = limitedAbility a step NoLimit Special NoCriteria cost (RunAbility (toTarget a) step)
  push $ SetActiveCost $ ActiveCost activeCostId ident (ForAbility ab) cost NoPayment Nothing mempty

costMessages ::
  ( MonadThrow m
  , HasGame m
  , MonadRandom m
  , HasQueue m
  , Projection m PlayerIdentity
  ) =>
  IdentityId ->
  ActiveCost ->
  m [Message]
costMessages iid activeCost = go (activeCostCost activeCost)
 where
  payer = activeCostIdentityId activeCost
  go = \case
    NoCost -> pure []
    DiscardHandCardCost n ->
      pure [IdentityMessage iid $ DiscardFrom FromHand n Nothing]
    DamageCost n -> case activeCostTarget activeCost of
      ForAbility a ->
        pure $
          damageIdentity iid (abilitySource a) (toDamage n FromAbility)
            <> [Paid $ DamagePayment n]
      _ -> error "Unhandled"
    HealCost n ->
      pure
        [ IdentityMessage iid $ IdentityHealed n
        , Paid $ HealPayment n
        ]
    DamageThisCost n -> case activeCostTarget activeCost of
      ForAbility a -> do
        pure $ case abilitySource a of
          AllyRef ident ->
            [ AllyMessage ident $
                AllyDamaged (abilitySource a) (toDamage n FromAbility)
            , Paid $ DamageThisPayment n
            ]
          _ -> error "Unhandled"
      _ -> error "Unhandled"
    ExhaustCost -> case activeCostTarget activeCost of
      ForAbility a -> do
        pure $ case abilitySource a of
          IdentityRef ident ->
            [ IdentityMessage ident ExhaustedIdentity
            , Paid ExhaustPayment
            ]
          AllyRef ident ->
            [ AllyMessage ident ExhaustedAlly
            , Paid ExhaustPayment
            ]
          SupportRef ident ->
            [ SupportMessage ident ExhaustedSupport
            , Paid ExhaustPayment
            ]
          UpgradeRef ident ->
            [ UpgradeMessage ident ExhaustedUpgrade
            , Paid ExhaustPayment
            ]
          AttachmentRef _ ->
            [ IdentityMessage iid ExhaustedIdentity
            , Paid ExhaustPayment
            ]
          _ -> error "Unhandled"
      _ -> error "Unhandled"
    DiscardCost target -> pure $ case target of
      UpgradeRef ident ->
        [ UpgradeMessage ident $ DiscardUpgrade
        , Paid $ DiscardPayment target
        ]
      _ -> error "Unhandled"
    UseCost -> case activeCostTarget activeCost of
      ForAbility a -> do
        pure $ case abilitySource a of
          UpgradeRef ident ->
            [ UpgradeMessage ident SpendUpgradeUse
            , Paid UsePayment
            ]
          SupportRef ident ->
            [ SupportMessage ident SpendSupportUse
            , Paid UsePayment
            ]
          AllyRef ident ->
            [ AllyMessage ident SpendAllyUse
            , Paid UsePayment
            ]
          _ -> error "Unhandled"
      _ -> error "Unhandled"
    DynamicResourceCost _ -> resourcePayment
    ResourceCost _ -> resourcePayment
    MultiResourceCost _ -> resourcePayment
    DifferentResourcesCost _ -> resourcePayment
    Costs xs -> concatMapM go (condenseResourceCosts xs)
  payingFor = case activeCostTarget activeCost of
    ForCard card -> Just (toTarget card)
    ForAbility a -> Just (refocus $ abilitySource a)
    ForTreachery -> Nothing
  resourcePayment = do
    cards <- getAvailablePaymentSources payer
    abilities <- getResourceAbilities payer $ case activeCostTarget activeCost of
      ForCard c -> Just c
      _ -> Nothing
    paid <- resourceCostPaid activeCost
    pure
      if
        | null cards && null abilities && not paid -> [AbortPayment (activeCostId activeCost)]
        -- nothing left to pay with, and nothing left to pay
        | null cards && null abilities -> [FinishedPayment (activeCostId activeCost)]
        | otherwise ->
          [ Ask payer $
              QuestionLabel "$label.pay" payingFor $
              ChooseOne $
                map PayWithCard cards
                  <> map UseAbility abilities
                  <> [FinishPayment | paid]
          ]

condenseResourceCosts :: [Cost] -> [Cost]
condenseResourceCosts xs = go xs False
 where
  go [] _ = []
  go (c@ResourceCost {} : cs) False = c : go cs True
  go (c@DynamicResourceCost {} : cs) False = c : go cs True
  go (c@MultiResourceCost {} : cs) False = c : go cs True
  go (c@DifferentResourcesCost {} : cs) False = c : go cs True
  go (ResourceCost {} : cs) True = go cs True
  go (DynamicResourceCost {} : cs) True = go cs True
  go (MultiResourceCost {} : cs) True = go cs True
  go (DifferentResourcesCost {} : cs) True = go cs True
  go (c : cs) b = c : go cs b

isResourceCost :: Cost -> Bool
isResourceCost (ResourceCost _) = True
isResourceCost (MultiResourceCost _) = True
isResourceCost (DynamicResourceCost _) = True
isResourceCost (DifferentResourcesCost _) = True
isResourceCost (Costs cs) = any isResourceCost cs
isResourceCost _ = False

onlyResourceCosts :: Cost -> Cost
onlyResourceCosts c@(ResourceCost _) = c
onlyResourceCosts c@(MultiResourceCost _) = c
onlyResourceCosts c@(DynamicResourceCost _) = c
onlyResourceCosts c@(DifferentResourcesCost _) = c
onlyResourceCosts (Costs cs) =
  case filter (/= NoCost) (map onlyResourceCosts cs) of
    [] -> NoCost
    xs -> Costs xs
onlyResourceCosts _ = NoCost

instance RunMessage ActiveCost where
  runMessage msg activeCost = case msg of
    CreatedActiveCost ident | ident == activeCostId activeCost -> do
      msgs <- costMessages (activeCostIdentityId activeCost) activeCost
      pushAll $ msgs <> [CheckPayment $ activeCostId activeCost]
      pure $
        activeCost
          { activeCostCost = onlyResourceCosts (activeCostCost activeCost)
          }
    Spent ident discard | ident == activeCostId activeCost -> do
      case activeCostTarget activeCost of
        ForCard card -> do
          resources <- resourcesFor discard $ Just card
          push $
            Paid $
              mconcat $
                map
                  ResourcePayment
                  resources
        ForAbility _ -> do
          resources <- resourcesFor discard Nothing
          push $
            Paid $
              mconcat $
                map
                  ResourcePayment
                  resources
        ForTreachery -> do
          resources <- resourcesFor discard Nothing
          push $
            Paid $
              mconcat $
                map
                  ResourcePayment
                  resources

      pure $
        activeCost
          { activeCostSpentCards = discard : activeCostSpentCards activeCost
          }
    Paid payment -> do
      pure $
        activeCost
          { activeCostPayment = activeCostPayment activeCost <> payment
          }
    -- cards <- getAvailablePaymentSources
    -- abilities <- getResourceAbilities
    -- paid <- resourceCostPaid activeCost'
    CheckPayment ident | ident == activeCostId activeCost -> do
      if isResourceCost (activeCostCost activeCost)
        then do
          msgs <- costMessages (activeCostIdentityId activeCost) activeCost
          pushAll $ msgs <> [CheckPayment $ activeCostId activeCost]
        else push $ FinishedPayment $ activeCostId activeCost
      pure activeCost
    WithDiscarded (ActiveCostRef ident) _ cards
      | ident == activeCostId activeCost -> do
          pushAll $
            map
              (Paid . DiscardHandCardPayment)
              cards
          pure activeCost
    AbortPayment ident | ident == activeCostId activeCost -> do
      cancelMatchingMessage (== (CheckPayment $ activeCostId activeCost))
      let payer = activeCostIdentityId activeCost
      pushAll $
        [IdentityMessage payer (AddToHand card) | ForCard card <- [activeCostTarget activeCost]]
          <> map (IdentityMessage payer . AddToHand) (activeCostSpentCards activeCost)
          <> [DisableActiveCost $ activeCostId activeCost]
      pure activeCost
    FinishedPayment ident | ident == activeCostId activeCost -> do
      cancelMatchingMessage (== (CheckPayment $ activeCostId activeCost))
      case activeCostTarget activeCost of
        ForCard card -> do
          let ident = activeCostIdentityId activeCost
          pushAll
            [ PutCardIntoPlay ident card (activeCostPayment activeCost) (activeCostWindow activeCost)
            , CheckWindows [Window After $ CardPlayed ident card]
            ]
        ForAbility ab -> do
          rest <-
            concatMapM
              (choiceMessages $ activeCostIdentityId activeCost)
              (abilityChoices ab)
          pushAll $ UsedAbility (activeCostIdentityId activeCost) ab : rest
        ForTreachery -> pure ()
      pushAll $
        map
          (DiscardedCard . PlayerCard)
          (reverse $ activeCostSpentCards activeCost)
          <> [DisableActiveCost $ activeCostId activeCost]
      pure activeCost
    _ -> pure activeCost

resourceCostPaid ::
  ( Projection m PlayerIdentity
  , HasGame m
  , HasQueue m
  , MonadRandom m
  , MonadThrow m
  ) =>
  ActiveCost ->
  m Bool
resourceCostPaid ActiveCost {..} = do
  let
    (rs, mrs) =
      first catMaybes $ partition isJust (costResources activeCostCost)
  prs <- paymentResources activeCostPayment
  flip evalStateT prs $ do
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
    pure $
      l
        && length prs' >= length mrs
        && coversDifferentTypes (differentResourcesNeeded activeCostCost) prs
