{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.ActiveCost (
  module Marvel.ActiveCost,
  module Marvel.ActiveCost.Types,
) where

import Marvel.Prelude

import Control.Monad.State
import Data.List (partition)
import Data.List qualified as L
import Marvel.Ability.Types
import Marvel.ActiveCost.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Discard
import Marvel.Hand
import Marvel.Game.Source
import Marvel.Id (CharacterId (..))
import Marvel.Identity.Types
import Marvel.Message.Lifted
import Marvel.Payment
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Queue (HasQueue)
import Marvel.Ref
import Marvel.Resource
import Marvel.Window.Types (Window (..), WindowTiming (After), WindowType (ResolvedAbility))


-- | Pay this part of the cost. Resources are paid first, so a cost that cannot be paid
-- in full is given up before anything else is paid.
payCostPart :: (ReverseQueue m) => ActiveCost -> Cost -> m ()
payCostPart activeCost = go
 where
  iid = activeCostIdentityId activeCost
  acid = activeCostId activeCost
  go = \case
    NoCost -> pure ()
    DiscardHandCardsCost n m -> do
      inHand <- projectMap PlayerIdentityHand (fromIntegral . length . unHand) iid
      discardFor iid (ActiveCostRef acid) FromHand n (fromMaybe inHand m)
    DamageCost n -> case activeCostTarget activeCost of
      ForAbility a -> do
        damageIdentity iid (abilitySource a) (toDamage n FromAbility)
        paid $ DamagePayment n
      _ -> error "Unhandled"
    HealCost n -> do
      heal iid n
      paid $ HealPayment n
    DamageThisCost n -> case activeCostTarget activeCost of
      ForAbility a -> case abilitySource a of
        AllyRef ally -> do
          damageCharacter ally (abilitySource a) (toDamage n FromAbility)
          paid $ DamageThisPayment n
        _ -> error "Unhandled"
      _ -> error "Unhandled"
    ExhaustCost -> case activeCostTarget activeCost of
      ForAbility a -> do
        case abilitySource a of
          IdentityRef ident -> exhaust ident
          AllyRef ally -> exhaust ally
          SupportRef support -> exhaust support
          UpgradeRef upgrade -> exhaust upgrade
          AttachmentRef _ -> exhaust iid
          ObligationRef _ -> exhaust iid
          _ -> error "Unhandled"
        paid ExhaustPayment
      _ -> error "Unhandled"
    DiscardCost target -> do
      case target of
        UpgradeRef upgrade -> discardUpgrade upgrade
        SupportRef _ -> removeFromPlay target
        _ -> error "Unhandled"
      paid $ DiscardPayment target
    UseCost -> case activeCostTarget activeCost of
      ForAbility a -> do
        case abilitySource a of
          UpgradeRef upgrade -> spendUse upgrade
          SupportRef support -> spendUse support
          AllyRef ally -> spendUse ally
          _ -> error "Unhandled"
        paid UsePayment
      _ -> error "Unhandled"
    ExhaustUpgradeCost matcher -> do
      upgrades <- selectList (matcher <> UnexhaustedUpgrade <> upgradeControlledBy iid)
      oneOf upgrades \u -> exhaust u >> paid ExhaustPayment
    ReturnUpgradeToHandCost matcher -> do
      upgrades <- selectList (matcher <> upgradeControlledBy iid)
      oneOf upgrades returnToHand
    -- one at a time, until the player is done or has exhausted as many as it allows
    ExhaustAlliesCost n m -> do
      allies <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId iid))
      when (maybe True (> 0) m && notNull allies) $
        chooseOnePromptedAbout iid "$label.chooseAllyToExhaust" (activeCostSource activeCost) $
          [labeled "$label.done" (pure ()) | n == 0]
            <> [ targeting a do
                  exhaust a
                  paid (ExhaustedAllyPayment a)
                  payCost acid (ExhaustAlliesCost (subtractNatural 1 n) (subtract 1 <$> m))
               | a <- allies
               ]
    ExhaustCharacterWithTraitCost t -> do
      you <- selectList (IdentityWithId iid <> UnexhaustedIdentity <> IdentityWithTrait t)
      allies <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId iid) <> AllyWithTrait t)
      chooseOrRunOne iid $
        [targeting i (exhaust i >> paid (ExhaustedCharacterPayment $ IdentityCharacter i)) | i <- you]
          <> [targeting a (exhaust a >> paid (ExhaustedCharacterPayment $ AllyCharacter a)) | a <- allies]
    DiscardEncounterCardsUntilMinionCost ->
      discardUntil FromEncounterDeck (CardWithType MinionType) (ActiveCostRef acid)
    -- added to the resources as the card was played
    TargetPrintedCost -> pure ()
    DynamicResourceCost _ -> resourcePayment
    ResourceCost _ -> resourcePayment
    MultiResourceCost _ -> resourcePayment
    DifferentResourcesCost _ -> resourcePayment
    Costs xs -> traverse_ go (condenseResourceCosts xs)
  payingFor = case activeCostTarget activeCost of
    ForTreachery -> chooseOnePrompted iid "$label.pay"
    _ -> chooseOnePromptedAbout iid "$label.pay" (activeCostSource activeCost)
  oneOf xs f = case xs of
    [x] -> f x
    [] -> pure ()
    _ -> chooseOne iid [targeting x (f x) | x <- xs]
  -- only what can be spent on it is offered: "you can only spend [physical] resources"
  resourcePayment = do
    let
      mcard = case activeCostTarget activeCost of
        ForCard card -> Just card
        _ -> Nothing
      spendable = any (canSpendOn $ maybe NoCriteria (cdCriteria . getCardDef) mcard)
    cards <- filterM (fmap spendable . (`resourcesFor` mcard)) =<< getAvailablePaymentSources iid
    abilities <- filterM (fmap spendable . abilityResources) =<< getResourceAbilities iid mcard
    isPaid <- resourceCostPaid activeCost
    if
      | null cards && null abilities && not isPaid -> abortPayment acid
      -- nothing left to pay with, and nothing left to pay
      | null cards && null abilities -> finishPayment acid
      | otherwise ->
          payingFor $
            map (pure . PayWithCard) cards
              <> map (pure . UseAbility) abilities
              <> [pure FinishPayment | isPaid]

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
  runMessage msg activeCost = runQueueT $ case msg of
    CreatedActiveCost ident | ident == activeCostId activeCost -> do
      checkPayment ident
      pure activeCost
    PayCost ident cost | ident == activeCostId activeCost -> do
      payCostPart activeCost cost
      pure activeCost
    Spent ident discard | ident == activeCostId activeCost -> do
      resources <- resourcesFor discard $ case activeCostTarget activeCost of
        ForCard card -> Just card
        _ -> Nothing
      paid $ mconcat $ map ResourcePayment resources
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
          payCostPart activeCost resources
          checkPayment ident
        else finishPayment ident
      pure activeCost
    WithDiscarded (ActiveCostRef ident) _ cards
      | ident == activeCostId activeCost -> do
          for_ cards (paid . DiscardHandCardPayment)
          pure activeCost
    WithDiscardedMatch (ActiveCostRef ident) _ (EncounterCard card)
      | ident == activeCostId activeCost -> do
          putEncounterCardIntoPlay (activeCostIdentityId activeCost) card
          pure activeCost
    AbortPayment ident | ident == activeCostId activeCost -> do
      cancelMatchingMessage (== CheckPayment ident)
      let payer = activeCostIdentityId activeCost
      -- a card played from a discard pile, or held by an upgrade, was left there
      fromHand <- case activeCostTarget activeCost of
        ForCard card -> do
          held <- getUpgradeHeldCards payer
          (card `notElem` held &&) <$> projectP PlayerIdentityDiscard (notElem card . unDiscard) payer
        _ -> pure False
      for_ [card | ForCard card <- [activeCostTarget activeCost], fromHand] (addToHand payer)
      for_ (activeCostSpentCards activeCost) (addToHand payer)
      disableActiveCost ident
      pure activeCost
    -- the resources are paid: then the rest of the cost
    FinishedPayment ident | ident == activeCostId activeCost -> do
      cancelMatchingMessage (== CheckPayment ident)
      payCostPart activeCost (otherCosts $ activeCostCost activeCost)
      paidCosts ident
      pure activeCost
    -- the cards spent are discarded and the cost done with, then what was paid for
    PaidCosts ident | ident == activeCostId activeCost -> do
      x <- spentX activeCost
      let
        payer = activeCostIdentityId activeCost
        payment = activeCostPayment activeCost <> x
      for_ (reverse $ activeCostSpentCards activeCost) discardCard
      disableActiveCost ident
      case activeCostTarget activeCost of
        ForCard card -> cardPlayed payer card payment (activeCostWindow activeCost)
        ForAbility ab -> do
          usedAbility payer ab
          for_ (abilityChoices ab) (resolveAs payer)
          def <- getsGame \g -> abilityCardDef g (refocus $ abilitySource ab)
          -- whatever the ability does, the window after it resolves follows
          for_ def \d -> checkWindows [Window After $ ResolvedAbility payer d]
        ForTreachery -> pure ()
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
