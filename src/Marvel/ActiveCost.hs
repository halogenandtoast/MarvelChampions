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
import Marvel.Criteria
import Marvel.Damage
import Marvel.Discard
import Marvel.Hand
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Message
import Marvel.Payment
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity, ExhaustedUpgrade)
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource
import Marvel.Window.Types (Window (..), WindowTiming (After), WindowType (CardPlayed, ResolvedAbility))


-- | What paying this part of the cost does. Resources are paid first, so a cost that
-- cannot be paid in full is given up before anything else is paid.
costMessages ::
  ( MonadThrow m
  , HasGame m
  , MonadRandom m
  , HasQueue m
  , Projection m PlayerIdentity
  ) =>
  ActiveCost ->
  Cost ->
  m [Message]
costMessages activeCost = go
 where
  iid = activeCostIdentityId activeCost
  acid = activeCostId activeCost
  go = \case
    NoCost -> pure []
    DiscardHandCardsCost n m -> do
      inHand <- projectMap PlayerIdentityHand (fromIntegral . length . unHand) iid
      pure [IdentityMessage iid $ DiscardFor (ActiveCostRef acid) FromHand n (fromMaybe inHand m)]
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
      SupportRef _ -> [RemoveFromPlay target, Paid $ DiscardPayment target]
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
    ExhaustUpgradeCost matcher -> do
      upgrades <- selectList (matcher <> UnexhaustedUpgrade <> upgradeControlledBy iid)
      pure $ oneOf upgrades \u -> [UpgradeMessage u ExhaustedUpgrade, Paid ExhaustPayment]
    ReturnUpgradeToHandCost matcher -> do
      upgrades <- selectList (matcher <> upgradeControlledBy iid)
      pure $ oneOf upgrades \u -> [ReturnToHand (toRef u)]
    -- one at a time, until the player is done or has exhausted as many as it allows
    ExhaustAlliesCost n m -> do
      allies <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId iid))
      pure
        [ Ask iid $
            promptedAbout "$label.chooseAllyToExhaust" (activeCostSource activeCost) $
              ChooseOne $
                [Label "$label.done" [] | n == 0]
                  <> [ TargetLabel
                      (toRef a)
                      [ Run
                          [ AllyMessage a ExhaustedAlly
                          , Paid (ExhaustedAllyPayment a)
                          , PayCost acid (ExhaustAlliesCost (subtractNatural 1 n) (subtract 1 <$> m))
                          ]
                      ]
                     | a <- allies
                     ]
        | maybe True (> 0) m
        , notNull allies
        ]
    DiscardEncounterCardsUntilMinionCost ->
      pure [DiscardUntil FromEncounterDeck (CardWithType MinionType) (ActiveCostRef acid)]
    -- added to the resources as the card was played
    TargetPrintedCost -> pure []
    DynamicResourceCost _ -> resourcePayment
    ResourceCost _ -> resourcePayment
    MultiResourceCost _ -> resourcePayment
    DifferentResourcesCost _ -> resourcePayment
    Costs xs -> concatMapM go (condenseResourceCosts xs)
  payingFor = case activeCostTarget activeCost of
    ForTreachery -> Nothing
    _ -> Just (refocus $ activeCostSource activeCost)
  oneOf xs f = case xs of
    [x] -> f x
    _ -> [Ask iid $ ChooseOne [TargetLabel (toRef x) [Run (f x)] | x <- xs]]
  -- only what can be spent on it is offered: "you can only spend [physical] resources"
  resourcePayment = do
    let
      mcard = case activeCostTarget activeCost of
        ForCard card -> Just card
        _ -> Nothing
      spendable = any (canSpendOn $ maybe NoCriteria (cdCriteria . getCardDef) mcard)
    cards <- filterM (fmap spendable . (`resourcesFor` mcard)) =<< getAvailablePaymentSources iid
    abilities <- filterM (fmap spendable . abilityResources) =<< getResourceAbilities iid mcard
    paid <- resourceCostPaid activeCost
    pure
      if
        | null cards && null abilities && not paid -> [AbortPayment acid]
        -- nothing left to pay with, and nothing left to pay
        | null cards && null abilities -> [FinishedPayment acid]
        | otherwise ->
          [ Ask iid $
              QuestionLabel "$label.pay" payingFor $
              ChooseOne $
                map PayWithCard cards
                  <> map UseAbility abilities
                  <> [FinishPayment | paid]
          ]

-- | What is being paid for.
activeCostSource :: ActiveCost -> Source
activeCostSource activeCost = case activeCostTarget activeCost of
  ForCard card -> toSource card
  ForAbility a -> abilitySource a
  ForTreachery -> GameRef

condenseResourceCosts :: [Cost] -> [Cost]
condenseResourceCosts xs = go xs False
 where
  go [] _ = []
  go (c : cs) paying
    | isResourceCost c = if paying then go cs True else c : go cs True
    | otherwise = c : go cs paying

isResourceCost :: Cost -> Bool
isResourceCost (ResourceCost _) = True
isResourceCost (MultiResourceCost _) = True
isResourceCost (DynamicResourceCost _) = True
isResourceCost (DifferentResourcesCost _) = True
isResourceCost (Costs cs) = any isResourceCost cs
isResourceCost _ = False

-- | The parts of the cost that are, or are not, resources.
resourceCosts, otherCosts :: Cost -> Cost
resourceCosts = filterCosts isResourceCost
otherCosts = filterCosts (not . isResourceCost)

filterCosts :: (Cost -> Bool) -> Cost -> Cost
filterCosts f = mconcat . filter f . flattenCosts

flattenCosts :: Cost -> [Cost]
flattenCosts = \case
  Costs cs -> concatMap flattenCosts cs
  NoCost -> []
  c -> [c]

-- | X, for a "spend X resources" cost paid with the rest of its card's or ability's
-- resources: what was spent of that type beyond the rest. Its payer divides the
-- resources between the costs, so whatever else could pay the rest does.
spentX ::
  ( Projection m PlayerIdentity
  , HasGame m
  , HasQueue m
  , MonadRandom m
  , MonadThrow m
  ) =>
  ActiveCost ->
  m Payment
spentX activeCost = case [r | DynamicResourceCost r <- flattenCosts (activeCostCost activeCost)] of
  [] -> pure NoPayment
  r : _ -> do
    resources <- paymentResources (activeCostPayment activeCost)
    let
      rest = length (costResources $ activeCostCost activeCost) - 1
      ofType = count (\x -> maybe True (\r' -> x `elem` [r', Wild]) r) resources
    pure $ XPayment $ min ofType (fromIntegral $ max 0 $ length resources - rest)

instance RunMessage ActiveCost where
  runMessage msg activeCost = case msg of
    CreatedActiveCost ident | ident == activeCostId activeCost -> do
      push $ CheckPayment $ activeCostId activeCost
      pure activeCost
    PayCost ident cost | ident == activeCostId activeCost -> do
      pushAll =<< costMessages activeCost cost
      pure activeCost
    Spent ident discard | ident == activeCostId activeCost -> do
      resources <- resourcesFor discard $ case activeCostTarget activeCost of
        ForCard card -> Just card
        _ -> Nothing
      push $ Paid $ mconcat $ map ResourcePayment resources
      pure $
        activeCost
          { activeCostSpentCards = discard : activeCostSpentCards activeCost
          }
    Paid payment -> do
      pure $
        activeCost
          { activeCostPayment = activeCostPayment activeCost <> payment
          }
    CheckPayment ident | ident == activeCostId activeCost -> do
      let resources = resourceCosts (activeCostCost activeCost)
      if resources /= NoCost
        then do
          msgs <- costMessages activeCost resources
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
    WithDiscardedMatch (ActiveCostRef ident) _ (EncounterCard card)
      | ident == activeCostId activeCost -> do
          push $ PutEncounterCardIntoPlay (activeCostIdentityId activeCost) card
          pure activeCost
    AbortPayment ident | ident == activeCostId activeCost -> do
      cancelMatchingMessage (== (CheckPayment $ activeCostId activeCost))
      let payer = activeCostIdentityId activeCost
      -- a card played from a discard pile was left there
      fromHand <- case activeCostTarget activeCost of
        ForCard card -> projectP PlayerIdentityDiscard (notElem card . unDiscard) payer
        _ -> pure False
      pushAll $
        [IdentityMessage payer (AddToHand card) | ForCard card <- [activeCostTarget activeCost], fromHand]
          <> map (IdentityMessage payer . AddToHand) (activeCostSpentCards activeCost)
          <> [DisableActiveCost $ activeCostId activeCost]
      pure activeCost
    -- the resources are paid: then the rest of the cost
    FinishedPayment ident | ident == activeCostId activeCost -> do
      cancelMatchingMessage (== (CheckPayment $ activeCostId activeCost))
      msgs <- costMessages activeCost (otherCosts $ activeCostCost activeCost)
      pushAll $ msgs <> [PaidCosts ident]
      pure activeCost
    PaidCosts ident | ident == activeCostId activeCost -> do
      x <- spentX activeCost
      let
        payer = activeCostIdentityId activeCost
        payment = activeCostPayment activeCost <> x
      case activeCostTarget activeCost of
        ForCard card ->
          pushAll
            [ IdentityMessage payer (PlayedCard card (activeCostWindow activeCost))
            , PutCardIntoPlay payer card payment (activeCostWindow activeCost)
            , CheckWindows [Window After $ CardPlayed payer card]
            ]
        ForAbility ab -> do
          rest <- concatMapM (choiceMessages payer) (abilityChoices ab)
          def <- getsGame \g -> abilityCardDef g (refocus $ abilitySource ab)
          -- whatever the ability does, the window after it resolves follows
          pushAll $
            UsedAbility payer ab
              : rest <> [CheckWindows [Window After $ ResolvedAbility payer d] | Just d <- [def]]
        ForTreachery -> pure ()
      pushAll $
        map
          (DiscardedCard . PlayerCard)
          (reverse $ activeCostSpentCards activeCost)
          <> [DisableActiveCost $ activeCostId activeCost]
      pure $ activeCost {activeCostPayment = payment}
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
