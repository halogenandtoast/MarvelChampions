{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.ActiveCost
  ( module Marvel.ActiveCost
  , module Marvel.ActiveCost.Types
  ) where

import Marvel.Prelude

import Data.List (partition)
import Data.List qualified as L
import Marvel.Ability.Types
import Marvel.ActiveCost.Types
import Marvel.Card
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
import Marvel.Resource
import Marvel.Source
import Marvel.Target

costMessages
  :: ( MonadThrow m
     , HasGame m
     , MonadRandom m
     , HasQueue m
     , Projection m PlayerIdentity
     )
  => IdentityId
  -> ActiveCost
  -> m [Message]
costMessages iid activeCost = go (activeCostCost activeCost)
 where
  go = \case
    NoCost -> pure []
    DiscardHandCardCost n ->
      pure [IdentityMessage iid $ DiscardFrom FromHand n Nothing]
    DamageCost n -> case activeCostTarget activeCost of
      ForAbility a -> pure
        [ IdentityMessage iid
          $ IdentityDamaged (abilitySource a) (toDamage n FromAbility)
        , Paid $ DamagePayment n
        ]
      _ -> error "Unhandled"
    HealCost n -> pure
      [ IdentityMessage iid $ IdentityHealed n
      , Paid $ HealPayment n
      ]
    DamageThisCost n -> case activeCostTarget activeCost of
      ForAbility a -> do
        pure $ case abilitySource a of
          AllySource ident ->
            [ AllyMessage ident
              $ AllyDamaged (abilitySource a) (toDamage n FromAbility)
            , Paid $ DamageThisPayment n
            ]
          _ -> error "Unhandled"
      _ -> error "Unhandled"
    ExhaustCost -> case activeCostTarget activeCost of
      ForAbility a -> do
        pure $ case abilitySource a of
          IdentitySource ident ->
            [ IdentityMessage ident ExhaustedIdentity
            , Paid ExhaustPayment
            ]
          AllySource ident ->
            [ AllyMessage ident ExhaustedAlly
            , Paid ExhaustPayment
            ]
          SupportSource ident ->
            [ SupportMessage ident ExhaustedSupport
            , Paid ExhaustPayment
            ]
          UpgradeSource ident ->
            [ UpgradeMessage ident ExhaustedUpgrade
            , Paid ExhaustPayment
            ]
          _ -> error "Unhandled"
      _ -> error "Unhandled"
    DiscardCost target -> pure $ case target of
      UpgradeTarget ident ->
        [ UpgradeMessage ident $ DiscardUpgrade
        , Paid $ DiscardPayment target
        ]
      _ -> error "Unhandled"
    UseCost -> case activeCostTarget activeCost of
      ForAbility a -> do
        pure $ case abilitySource a of
          UpgradeSource ident ->
            [ UpgradeMessage ident SpendUpgradeUse
            , Paid UsePayment
            ]
          SupportSource ident ->
            [ SupportMessage ident SpendSupportUse
            , Paid UsePayment
            ]
          AllySource ident ->
            [ AllyMessage ident SpendAllyUse
            , Paid UsePayment
            ]
          _ -> error "Unhandled"
      _ -> error "Unhandled"
    DynamicResourceCost _ -> do
      cards <- getAvailablePaymentSources
      abilities <- getResourceAbilities
      paid <- resourceCostPaid activeCost
      pure
        [ Ask (activeCostIdentityId activeCost)
          $ ChooseOne
          $ map PayWithCard cards
          <> map UseAbility abilities
          <> [ FinishPayment | paid ]
        ]
    ResourceCost _ -> do
      cards <- getAvailablePaymentSources
      abilities <- getResourceAbilities
      paid <- resourceCostPaid activeCost
      pure
        [ Ask (activeCostIdentityId activeCost)
          $ ChooseOne
          $ map PayWithCard cards
          <> map UseAbility abilities
          <> [ FinishPayment | paid ]
        ]
    MultiResourceCost _ -> do
      cards <- getAvailablePaymentSources
      abilities <- getResourceAbilities
      paid <- resourceCostPaid activeCost
      pure
        [ Ask (activeCostIdentityId activeCost)
          $ ChooseOne
          $ map PayWithCard cards
          <> map UseAbility abilities
          <> [ FinishPayment | paid ]
        ]
    Costs xs -> concatMapM go (condenseResourceCosts xs)

condenseResourceCosts :: [Cost] -> [Cost]
condenseResourceCosts xs = go xs False
 where
  go [] _ = []
  go (c@ResourceCost{} : cs) False = c : go cs True
  go (c@DynamicResourceCost{} : cs) False = c : go cs True
  go (c@MultiResourceCost{} : cs) False = c : go cs True
  go (ResourceCost{} : cs) True = go cs True
  go (DynamicResourceCost{} : cs) True = go cs True
  go (MultiResourceCost{} : cs) True = go cs True
  go (c : cs) b = c : go cs b

isResourceCost :: Cost -> Bool
isResourceCost (ResourceCost _) = True
isResourceCost (MultiResourceCost _) = True
isResourceCost (DynamicResourceCost _) = True
isResourceCost (Costs cs) = any isResourceCost cs
isResourceCost _ = False

onlyResourceCosts :: Cost -> Cost
onlyResourceCosts c@(ResourceCost _) = c
onlyResourceCosts c@(MultiResourceCost _) = c
onlyResourceCosts c@(DynamicResourceCost _) = c
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
      pure $ activeCost
        { activeCostCost = onlyResourceCosts (activeCostCost activeCost)
        }
    Spent ident discard | ident == activeCostId activeCost -> do
      case activeCostTarget activeCost of
        ForCard card -> do
          resources <- resourcesFor discard $ Just card
          push $ Paid $ mconcat $ map
            ResourcePayment
            resources
        ForAbility _ -> do
          resources <- resourcesFor discard Nothing
          push $ Paid $ mconcat $ map
            ResourcePayment
            resources
        ForTreachery -> do
          resources <- resourcesFor discard Nothing
          push $ Paid $ mconcat $ map
            ResourcePayment
            resources

      pure $ activeCost
        { activeCostSpentCards = discard : activeCostSpentCards activeCost
        }
    Paid payment -> do
      pure $ activeCost
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
    WithDiscarded (ActiveCostTarget ident) _ cards
      | ident == activeCostId activeCost -> do
        pushAll $ map
          (Paid . DiscardHandCardPayment)
          cards
        pure activeCost
    FinishedPayment ident | ident == activeCostId activeCost -> do
      cancelMatchingMessage (== (CheckPayment $ activeCostId activeCost))
      case activeCostTarget activeCost of
        ForCard card -> do
          push $ PutCardIntoPlay
            (activeCostIdentityId activeCost)
            card
            (activeCostPayment activeCost)
            (activeCostWindow activeCost)
        ForAbility ab -> do
          rest <- concatMapM
            (choiceMessages $ activeCostIdentityId activeCost)
            (abilityChoices ab)
          pushAll $ UsedAbility (activeCostIdentityId activeCost) ab : rest
        ForTreachery -> pure ()
      pushAll
        $ map
            (DiscardedCard . PlayerCard)
            (reverse $ activeCostSpentCards activeCost)
        <> [DisableActiveCost $ activeCostId activeCost]
      pure activeCost
    _ -> pure activeCost

resourceCostPaid
  :: ( Projection m PlayerIdentity
     , HasGame m
     , HasQueue m
     , MonadRandom m
     , MonadThrow m
     )
  => ActiveCost
  -> m Bool
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
    pure $ l && length prs' >= length mrs

sourceToTarget :: Source -> Target
sourceToTarget = \case
  IdentitySource ident -> IdentityTarget ident
  VillainSource ident -> VillainTarget ident
  MinionSource ident -> MinionTarget ident
  AllySource ident -> AllyTarget ident
  EventSource ident -> EventTarget ident
  EffectSource ident -> EffectTarget ident
  SupportSource ident -> SupportTarget ident
  TreacherySource ident -> TreacheryTarget ident
  ObligationSource ident -> ObligationTarget ident
  SideSchemeSource ident -> SideSchemeTarget ident
  MainSchemeSource ident -> MainSchemeTarget ident
  AttachmentSource ident -> AttachmentTarget ident
  UpgradeSource ident -> UpgradeTarget ident
  GameSource -> error "unhandled"
