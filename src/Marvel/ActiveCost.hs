{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.ActiveCost (
  module Marvel.ActiveCost,
  module Marvel.ActiveCost.Types,
) where

import Marvel.Prelude
import Control.Monad (replicateM_)
import System.Random.Shuffle (shuffleM)
import Marvel.Modifier.Types (Modifier (CannotBeDiscarded, CannotTakeDamage, CannotTakeDamageFromPlayerCards))

import Control.Monad.State
import Data.List (partition, nub)
import Data.List qualified as L
import Marvel.Ability.Types
import Marvel.ActiveCost.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Game.Source
import Marvel.I18n (ikeyWith, intVar)
import Marvel.Id (CharacterId (..), IdentityId, MinionId (..))
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
import Marvel.Trait (Trait)
import Marvel.Resource.Resources (afterSpent)
import Marvel.Window (windowMatches)
import Marvel.Window.Types (Window (..), WindowTiming (After), WindowType (PlayedCardFromHand, ResolvedAbility, GeneratedResources))


-- | Pay this part of the cost. Resources are paid first, so a cost that cannot be paid
-- in full is given up before anything else is paid.
payCostPart :: (ReverseQueue m) => ActiveCost -> Cost -> m ()
payCostPart = payCostPartAs False

payCostPartAs :: (ReverseQueue m) => Bool -> ActiveCost -> Cost -> m ()
payCostPartAs contributing activeCost cost = do
  payers <- allianceCostPayers iid mcard
  if not contributing && length payers > 1 && isDelegable cost
    then do
      eligible <- case cost of
        ExhaustCharacterWithTraitCost t -> do
          valid <- validExhaustions activeCost t
          filterM (\p -> any (`elem` valid) <$> payerCharacters p t) payers
        _ -> filterM (\p -> canAffordCost p (activeCostSource activeCost) cost) payers
      case eligible of
        [] -> abortPayment acid
        [p] -> contributeCost acid p cost
        _ -> chooseOnePromptedAbout iid "$label.chooseCostContributor" (activeCostSource activeCost)
          [targeting p (contributeCost acid p cost) | p <- eligible]
    else go cost
 where
  mcard = case activeCostTarget activeCost of
    ForCard card -> Just card
    _ -> Nothing
  isDelegable = \case
    Costs _ -> False
    NoCost -> False
    ChosenHandCardCost -> False
    ChooseCharacterWithTraitCost _ _ -> False
    c -> not (isResourceCost c)

  iid = activeCostIdentityId activeCost
  acid = activeCostId activeCost
  go = \case
    NoCost -> pure ()
    DiscardHandCardsCost n m -> do
      inHand <- projectMap PlayerIdentityHand (fromIntegral . length . unHand) iid
      discardFor iid (ActiveCostRef acid) FromHand n (fromMaybe inHand m)
    DiscardHandCardsWithTotalCost matcher n -> cardSpecific (ActiveCostRef acid) "discardTotalCost" (matcher, n)
    ExactCountersCost n -> do
      count <- getCountersOn (activeCostSource activeCost)
      if count < n then abortPayment acid else removeCounters (activeCostSource activeCost) n >> paid (CountersPayment n)
    GiveEnemyToughCost enemy -> do
      tough <- characterMatches (CharacterWithStatus ToughStatus) (toCharacter enemy)
      if tough then abortPayment acid else enemyBecomesTough enemy
    GiveEnemyBoostCost enemy n -> replicateM_ (fromIntegral n) (dealBoost $ enemyRef enemy)
    DiscardRandomHandCardsCost n -> discardFrom iid RandomFromHand n
    DiscardRandomHandCardsMatchingCost n matcher -> do
      hand <- projectMap PlayerIdentityHand unHand iid
      let candidates = filter (\c -> cardMatch matcher c && not (staysInHand c) && activeCostSource activeCost /= toSource c) hand
      if length candidates < fromIntegral n
        then abortPayment acid
        else do
          discarded <- take (fromIntegral n) <$> shuffleM candidates
          for_ discarded \card -> do
            discardCard card
            paid (DiscardHandCardPayment $ PlayerCard card)
    IndirectDamageCost n -> cardSpecific (ActiveCostRef acid) "assignIndirectCost" (n, [] :: [(CharacterId, Natural)])
    DamageCharacterCost matcher n -> do
      candidates <- selectList matcher
      valid <- filterM (fmap (\ms -> CannotTakeDamage `notElem` ms && CannotTakeDamageFromPlayerCards `notElem` ms) . getModifiers . characterRef) candidates
      chooseOne iid [targeting (characterRef c) (damageCharacter c (activeCostSource activeCost) (toDamage n FromAbility) >> paid (ChosenCharacterPayment c <> DamagePayment n)) | c <- valid]
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
          MainSchemeRef _ -> exhaust iid
          _ -> error "Unhandled"
        paid ExhaustPayment
      ForCard _ -> exhaust iid >> paid ExhaustPayment
      _ -> error "Unhandled"
    RemoveFromGameCost target -> do
      removeFromGame target
      paid (DiscardPayment target)
    DiscardControlledCardCost matcher -> do
      targets <- discardableControlledCardsMatching iid matcher
      if null targets then abortPayment acid else oneOf targets \target -> do
        discardControlled target
        paid (DiscardPayment target)
    DiscardCost target -> do
      case target of
        UpgradeRef upgrade -> discardUpgrade upgrade
        SupportRef _ -> removeFromPlay target
        AllyRef _ -> removeFromPlay target
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
    ExhaustSupportCost matcher -> do
      supports <- selectList (SupportMatches [matcher, UnexhaustedSupport, SupportControlledBy (IdentityWithId iid)])
      oneOf supports (\s -> exhaust s >> paid (ExhaustedCardPayment $ toTarget s))
    ExhaustUpgradeCost matcher -> do
      upgrades <- selectList (matcher <> UnexhaustedUpgrade <> upgradeControlledBy iid)
      oneOf upgrades \u -> exhaust u >> paid ExhaustPayment
    ReturnUpgradeToHandCost matcher -> do
      upgrades <- selectList (matcher <> upgradeControlledBy iid)
      oneOf upgrades returnToHand
    DiscardUpgradeCost matcher -> do
      upgrades <- selectList (matcher <> UpgradeThatCanBeDiscarded <> upgradeControlledBy iid)
      oneOf upgrades \u -> do
        card <- getsGame (\g -> cardInPlay g (UpgradeRef u))
        for_ card (paid . DiscardedUpgradePayment)
        discardUpgrade u
        paid (DiscardPayment $ toTarget u)
    DiscardAllyCost matcher -> do
      allies <- selectList (matcher <> AllyControlledBy (IdentityWithId iid))
      oneOf allies \aid -> do
        card <- getsGame $ \g -> cardInPlay g (AllyRef aid)
        for_ card (paid . DiscardedAllyPayment)
        removeFromPlay aid
    DiscardSupportCost matcher -> do
      supports <- filterM discardable =<< selectList (SupportMatches [matcher, SupportControlledBy (IdentityWithId iid)])
      if null supports then abortPayment acid else oneOf supports \sid -> do
        removeFromPlay sid
        paid (DiscardPayment $ toTarget sid)
    DiscardAllyOrSupportCost allyMatcher supportMatcher -> do
      allies <- filterM discardable =<< selectList (allyMatcher <> AllyControlledBy (IdentityWithId iid))
      supports <- filterM discardable =<< selectList (SupportMatches [supportMatcher, SupportControlledBy (IdentityWithId iid)])
      if null allies && null supports
        then abortPayment acid
        else chooseOne iid $
          [targeting aid do
              card <- getsGame (\g -> cardInPlay g (AllyRef aid))
              for_ card (paid . DiscardedAllyPayment)
              removeFromPlay aid
              paid (DiscardPayment $ toTarget aid)
           | aid <- allies]
            <> [targeting sid (removeFromPlay sid >> paid (DiscardPayment $ toTarget sid)) | sid <- supports]
    DiscardTopOfDeckCost -> discardTopOfDeck iid 1
    -- the cards discarded are paid with ('paymentDiscards'); none discarded, none paid
    DiscardTopOfDeckCardsCost lo hi -> do
      inDeck <- projectMap PlayerIdentityDeck (fromIntegral . length . unDeck) iid
      chooseOnePromptedAbout iid "$label.discardFromTopOfDeck" (activeCostSource activeCost)
        [ labeled (countLabel "discardTopOfDeck" k) (when (k > 0) $ discardFromDeck iid k (ActiveCostRef acid))
        | k <- [lo .. min hi inDeck]
        ]
    DiscardTopOfDeckForDamageCost -> do
      n <- pendingDamageAmount iid
      when (n > 0) $ discardFromDeck iid n (ActiveCostRef acid)
    -- one at a time, until the player is done or has exhausted as many as it allows
    ExhaustCardsWithTraitCost trait lo hi -> do
      cards <- readyTraitCardCandidates iid trait
      if length cards < fromIntegral lo
        then abortPayment acid
        else when (hi > 0 && notNull cards) $
          chooseOne iid $
            [labeled "$label.done" (pure ()) | lo == 0]
              <> [targeting target do
                    case target of
                      IdentityRef p -> exhaust p
                      AllyRef a -> exhaust a
                      SupportRef s -> exhaust s
                      UpgradeRef u -> exhaust u
                      _ -> pure ()
                    paid (ExhaustedCardPayment target)
                    payCost acid (ExhaustCardsWithTraitCost trait (subtractNatural 1 lo) (hi - 1))
                 | target <- cards]
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
    ExhaustAlliesMatchingCost matcher n -> do
      allies <- selectList (matcher <> UnexhaustedAlly <> AllyControlledBy (IdentityWithId iid))
      when (n > 0) $
        chooseOnePromptedAbout iid "$label.chooseAllyToExhaust" (activeCostSource activeCost) $
          [ targeting a do
              exhaust a
              paid (ExhaustedAllyPayment a)
              payCost acid (ExhaustAlliesMatchingCost matcher (n - 1))
          | a <- allies
          ]
    ExhaustCharacterCost matcher -> do
      candidates <- readyCharacterCandidates iid matcher
      if null candidates
        then abortPayment acid
        else chooseOne iid
          [ targeting (characterRef character) do
              case character of
                IdentityCharacter ident -> exhaust ident
                AllyCharacter ally -> exhaust ally
                _ -> pure ()
              paid (ExhaustedCharacterPayment character)
          | character <- candidates
          ]
    ExhaustCharacterWithTraitCost t -> do
      valid <- validExhaustions activeCost t
      you <- selectList (IdentityWithId iid <> UnexhaustedIdentity <> IdentityWithTrait t)
      allies <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId iid) <> AllyWithTrait t)
      let choices =
            [targeting i (exhaust i >> paid (ExhaustedCharacterPayment $ IdentityCharacter i)) | i <- you, IdentityCharacter i `elem` valid]
              <> [targeting a (exhaust a >> paid (ExhaustedCharacterPayment $ AllyCharacter a)) | a <- allies, AllyCharacter a `elem` valid]
      if null choices then abortPayment acid else chooseOne iid choices
    RevealEncounterTreacheryCost -> do
      deck <- getEncounterDeck
      discardUntil FromEncounterDeck (CardWithType TreacheryType) (ActiveCostRef acid)
      unless (any (cardMatch $ CardWithType TreacheryType) deck) (abortPayment acid)
    ShuffleEncounterCardsIntoOwnDeckCost n -> do
      enough <- (>= n) . fromIntegral . length <$> ((<>) <$> getEncounterDeck <*> getEncounterDiscard)
      if enough then shuffleTopEncounterCardsIntoPlayerDeck iid n else abortPayment acid
    DiscardEncounterCardsUntilMinionCost ->
      discardUntil FromEncounterDeck (CardWithType MinionType) (ActiveCostRef acid)
    SearchEncounterSideSchemeCost -> do
      deck <- getEncounterDeck
      let found = filter (cardMatch $ CardWithType SideSchemeType) deck
      if null found then abortPayment acid else
        chooseOneSearchedIn iid "$label.searchEncounterDeck" [("$label.encounterDeck", map EncounterCard deck)]
          [targeting card (revealEncounterCard iid card >> shuffleEncounterDeck) | card <- found]
    SearchEncounterMinionCost -> do
      deck <- getEncounterDeck
      discard <- getEncounterDiscard
      let found = filter (cardMatch $ CardWithType MinionType) (deck <> discard)
      if null found then abortPayment acid else
        chooseOneSearchedIn iid "$label.searchEncounterDeck" [("$label.encounterDeck", map EncounterCard deck), ("$label.encounterDiscard", map EncounterCard discard)]
          [targeting card (paid (SearchedMinionPayment $ MinionId $ unCardId $ ecCardId card) >> putEncounterCardIntoPlay iid card >> shuffleEncounterDeck) | card <- found]
    PlaceThreatOnMainSchemeCost n -> traverse_ (`placeThreat` n) =<< selectList MainScheme
    DealEncounterCardCost -> dealEncounterCard iid
    IdentityCountersCost n -> removeCounters (IdentityRef iid) n
    CountersFromThisCost n -> do
      let source = activeCostSource activeCost
      most <- getCountersOn source
      chooseOne iid
        [ labeled (countLabel "discardCounters" k) (removeCounters source k >> paid (CountersPayment k))
        | k <- [max 1 n .. most]
        ]
    DiscardTopEncounterCardCost -> do
      top <- take 1 <$> getEncounterDeck
      discardTopOfEncounterDeck 1
      for_ top (paid . DiscardedEncounterCardPayment . EncounterCard)
    -- added to the resources as the card was played
    TargetPrintedCost -> pure ()
    ChosenHandCardCost -> pure ()
    ChooseCharacterWithTraitCost trait matcher -> do
      characters <- validCharacterChoices activeCost trait matcher
      if null characters
        then abortPayment acid
        else chooseOne iid
          [targeting (characterRef character) (paid $ ChosenCharacterPayment character) | character <- characters]
    DynamicResourceCost _ -> resourcePayment iid contributing
    ResourceCost _ -> resourcePayment iid contributing
    MultiResourceCost _ -> resourcePayment iid contributing
    PrintedResourceCost _ -> resourcePayment iid contributing
    DifferentResourcesCost _ -> resourcePayment iid contributing
    MayOverpay -> resourcePayment iid contributing
    PrintedXCost _ _ -> pure ()
    UpToResourcesCost _ -> resourcePayment iid contributing
    ResourcesFromHandCost _ -> resourcePayment iid contributing
    ExhaustMilanoCost -> exhaustMilano >> paid ExhaustPayment
    IfControlsMilanoCost c c' -> controlsMilano iid >>= \m -> go (if m then c else c')
    Costs xs -> traverse_ (payCost acid) (condenseResourceCosts xs)
  payingFor payer = case activeCostTarget activeCost of
    ForTreachery -> chooseOnePrompted payer "$label.pay"
    _ -> chooseOnePromptedAbout payer "$label.pay" (activeCostSource activeCost)
  discardable target = notElem CannotBeDiscarded <$> getModifiers target
  oneOf :: (ReverseQueue n, IsRef a) => [a] -> (a -> n ()) -> n ()
  oneOf xs f = case xs of
    [x] -> f x
    [] -> pure ()
    _ -> chooseOne iid [targeting x (f x) | x <- xs]
  -- only what can be spent on it is offered: "you can only spend [physical] resources"
  resourcePayment payer contributor = do
    let
      mcard = case activeCostTarget activeCost of
        ForCard card -> Just card
        _ -> Nothing
      spendable = any (canSpendOn $ maybe NoCriteria (cdCriteria . getCardDef) mcard)
    -- a card's ability used from hand (Pip the Troll) is not paid for with that card
    let notTheSource c = activeCostSource activeCost /= CardIdRef (pcCardId c)
          && (ChosenHandCardCost `notElem` flattenCosts (activeCostCost activeCost)
              || paymentTarget (activeCostPayment activeCost) /= Just (CardIdRef $ pcCardId c))
    originalPayer <- fromMaybe iid <$> getActiveCostPayer acid
    cards <- filterM (fmap spendable . (`resourcesFor` mcard)) . filter notTheSource =<< getPaymentSourcesFor originalPayer payer mcard
    -- "from your hand": the cards in it, none of the resource abilities in play
    let printed = printedResourceRequirements (activeCostCost activeCost)
        printedOnly = notNull printed && length printed == length (costResources $ activeCostCost activeCost)
        fromHandOnly = printedOnly || notNull [() | ResourcesFromHandCost _ <- flattenCosts (activeCostCost activeCost)]
    abilities <-
      if fromHandOnly
        then pure []
        else filterM (fmap spendable . abilityResources) =<< getPaymentResourceAbilities originalPayer payer mcard
    let canHelp p = do
          cs <- filterM (fmap spendable . (`resourcesFor` mcard)) . filter notTheSource =<< getPaymentSourcesFor iid p mcard
          abs' <- if fromHandOnly then pure [] else filterM (fmap spendable . abilityResources) =<< getPaymentResourceAbilities iid p mcard
          pure $ notNull cs || notNull abs'
    payers <- resourceCostPayers iid mcard
    helpers <- if contributor then pure [] else filterM canHelp (filter (/= iid) payers)
    isPaid <- resourceCostPaid activeCost
    resources <- paymentResources (activeCostPayment activeCost)
    let upper = headMay [n | UpToResourcesCost n <- flattenCosts (activeCostCost activeCost)]
        atCap = isPaid && maybe False (\n -> length resources >= length (costResources $ activeCostCost activeCost) - 1 + fromIntegral n) upper
        payableCards = if atCap then [] else cards
        payableAbilities = if atCap then [] else abilities
        payableHelpers = if atCap then [] else helpers
    let
      wilds = paymentWilds (activeCostPayment activeCost)
      declares = [DeclareWild n r | (n, (owner, was)) <- zip [0 ..] (paymentWildOwners $ activeCostPayment activeCost), isNothing owner, r <- [Energy, Mental, Physical, Wild], r /= was]
    if
      | contributor -> payingFor payer $
          map (pure . PayWithCard) payableCards <> map (pure . UseAbility) payableAbilities <> [labeled "$label.doneContributing" (pure ())]
      | null payableCards && null payableAbilities && null payableHelpers && not isPaid -> abortPayment acid
      -- nothing left to pay with, nothing left to pay, and no wild to declare
      | null payableCards && null payableAbilities && null wilds && isPaid -> finishPayment acid
      | otherwise ->
          payingFor payer $
            map (pure . PayWithCard) payableCards
              <> map (pure . UseAbility) payableAbilities
              <> [targeting p (contributeResources acid p) | p <- payableHelpers]
              <> [labeled "$label.cancelPayment" (abortPayment acid) | not isPaid && notNull helpers]
              <> map pure declares
              <> [pure FinishPayment | isPaid]

-- | Ready characters belonging to a contributor with the required current trait.
payerCharacters :: (HasGame m) => IdentityId -> Trait -> m [CharacterId]
payerCharacters p t = do
  heroes <- selectList (IdentityWithId p <> UnexhaustedIdentity <> IdentityWithTrait t)
  allies <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId p) <> AllyWithTrait t)
  pure $ map IdentityCharacter heroes <> map AllyCharacter allies

-- | Only offer exhaustions that leave a way to pay the remaining character costs.
-- In particular, choosing a dual Avenger/Guardian must not strand the other cost.
validExhaustions :: (HasGame m) => ActiveCost -> Trait -> m [CharacterId]
validExhaustions cost t = do
  let card = case activeCostTarget cost of ForCard c -> Just c; _ -> Nothing
  payers <- allianceCostPayers (activeCostIdentityId cost) card
  choices <- concatMapM (`payerCharacters` t) payers
  let assigned = case paymentApprovedAlliance (activeCostPayment cost) of
        Just approved -> [(p, trait) | (p, c) <- approved, ExhaustCharacterWithTraitCost trait <- flattenCosts c]
        Nothing -> []
      afterThis = drop (1 + length (paymentExhaustedCharacters $ activeCostPayment cost)) assigned
  rest <- if null assigned
    then traverse (\trait -> concatMapM (`payerCharacters` trait) payers) remaining
    else traverse (uncurry payerCharacters) afterThis
  let past = paymentExhaustedCharacters (activeCostPayment cost)
      allocates used [] = validEventExhaustions cost (past <> reverse used)
      allocates used (xs : xss) = anyM (\x ->
        if x `elem` (past <> used) then pure False else allocates (x : used) xss) xs
  filterM (\c -> if c `elem` past then pure False else allocates [c] rest) choices
 where
  required = [trait | ExhaustCharacterWithTraitCost trait <- flattenCosts (activeCostCost cost)]
  remaining = drop (1 + length (paymentExhaustedCharacters $ activeCostPayment cost)) required

-- | Check a proposed allocation before committing any contributions. An unknown
-- payer may be any eligible player; assigned trait exhaustions must be distinct.
canAffordContributionPlan :: (ReverseQueue m) => ActiveCost -> [IdentityId] -> [(Maybe IdentityId, Cost)] -> m Bool
canAffordContributionPlan cost payers proposed = do
  traits <- traverse (\(who, t) -> concatMapM (`payerCharacters` t) (maybe payers pure who)) traitParts
  chosen <- traverse (uncurry chooseCharacterTraitCandidates . snd) chooseParts
  others <- allM (\(who, c) -> anyM (\p -> canAffordCost p source c) (maybe payers pure who)) otherParts
  let assign _ [] = True
      assign used (xs : rest) = any (\x -> x `notElem` used && assign (x : used) rest) xs
      assignExhaustions used [] = validEventExhaustions cost (reverse used)
      assignExhaustions used (xs : rest) = anyM (\x ->
        if x `elem` used then pure False else assignExhaustions (x : used) rest) xs
  meaningful <- assignExhaustions [] traits
  pure $ others && meaningful && assign [] chosen
 where
  source = activeCostSource cost
  parts = [(who, c) | (who, cost) <- proposed, c <- flattenCosts cost]
  traitParts = [(who, t) | (who, ExhaustCharacterWithTraitCost t) <- parts]
  chooseParts = [(who, (t, m)) | (who, ChooseCharacterWithTraitCost t m) <- parts]
  otherParts = [(who, c) | (who, c) <- parts, case c of ExhaustCharacterWithTraitCost _ -> False; ChooseCharacterWithTraitCost _ _ -> False; _ -> True]

-- | A wild resource paid is one its payer may declare as a type, as they generate it
-- (RR Wild Resource): a card's printed resources are read as it is paid with.
declarable ::
  ( Projection m PlayerIdentity
  , HasGame m
  , HasQueue m
  , MonadRandom m
  , MonadThrow m
  ) =>
  Payment ->
  m Payment
declarable = fmap mconcat . traverse one . payments
 where
  one = \case
    ResourcePayment Wild -> pure $ WildPayment Wild
    p@(ResourcePaymentFromCard _) -> mconcat . map asPaid <$> paymentResources p
    p -> pure p
  asPaid = \case
    Wild -> WildPayment Wild
    r -> ResourcePayment r

-- | What is being paid for.
activeCostSource :: ActiveCost -> Source
activeCostSource activeCost = case activeCostTarget activeCost of
  ForCard card -> toSource card
  ForAbility a -> abilitySource a
  ForTreachery -> GameRef
  ForContinuation source _ -> source

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
isResourceCost (PrintedResourceCost _) = True
isResourceCost (DynamicResourceCost _) = True
isResourceCost (DifferentResourcesCost _) = True
isResourceCost MayOverpay = True
isResourceCost (UpToResourcesCost _) = True
isResourceCost (ResourcesFromHandCost _) = True
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
spentX activeCost = case dynamicCosts <> boundedCosts of
  [] -> pure NoPayment
  (r, minimumSpent, cap) : _ -> do
    resources <- paymentResources (activeCostPayment activeCost)
    let
      rest = length (costResources $ activeCostCost activeCost) - minimumSpent
      ofType = count (\x -> maybe True (\r' -> x `elem` [r', Wild]) r) resources
      extra = min ofType (fromIntegral $ max 0 $ length resources - rest)
    pure $ XPayment $ maybe extra (min extra) cap
 where
  parts = flattenCosts (activeCostCost activeCost)
  dynamicCosts = [(r, 1, Nothing) | DynamicResourceCost r <- parts]
  boundedCosts = [(Nothing, 1, Just n) | UpToResourcesCost n <- parts]

instance RunMessage ActiveCost where
  runMessage msg activeCost
    -- a payer defeated while paying (Venom's Symbiotic Bond paid with his last hit
    -- point) pays nothing more, and gets nothing back: the cost is dropped
    | costStep = do
        alive <- identityMatches (IdentityWithId payer) payer
        if alive
          then runActiveCost msg activeCost
          else runQueueT (activeCost <$ disableActiveCost acid)
    | otherwise = runActiveCost msg activeCost
   where
    payer = activeCostIdentityId activeCost
    acid = activeCostId activeCost
    costStep = case msg of
      CheckPayment i -> i == acid
      AbortPayment i -> i == acid
      FinishedPayment i -> i == acid
      AllocatedPayment i _ -> i == acid
      CardSpecific (ActiveCostRef i) "allocateSyncRatio" _ -> i == acid
      PaidCosts i -> i == acid
      PayCost i _ -> i == acid
      _ -> False

runActiveCost ::
  (HasCallStack, MonadThrow m, MonadRandom m, HasGame m, HasQueue m, Projection m PlayerIdentity) =>
  Message ->
  ActiveCost ->
  m ActiveCost
runActiveCost msg activeCost = runQueueT $ case msg of
    CreatedActiveCost ident | ident == activeCostId activeCost -> do
      checkPayment ident
      pure activeCost
    ContributeResources ident contributor | ident == activeCostId activeCost -> do
      allowed <- resourceCostPayers (activeCostIdentityId activeCost) $ case activeCostTarget activeCost of
        ForCard card -> Just card
        _ -> Nothing
      when (contributor `elem` allowed) do
        for_ (headMay $ flattenCosts $ resourceCosts $ activeCostCost activeCost) $
          payCostPartAs True (activeCost {activeCostIdentityId = contributor})
        declareContributionWilds ident contributor (fromIntegral $ length $ paymentWilds $ activeCostPayment activeCost)
      pure activeCost
    DeclareContributionWilds ident contributor start | ident == activeCostId activeCost -> do
      let payment = attributeWilds start contributor (activeCostPayment activeCost)
          newly = drop (fromIntegral start) (zip [0 ..] $ paymentWilds payment)
      for_ newly \(n, _) -> chooseOnePrompted contributor "$label.declareContributedWild"
        [pure (DeclareWild n r) | r <- [Energy, Mental, Physical, Wild]]
      pure activeCost {activeCostPayment = payment}
    PrepareAllianceCosts ident approved remaining | ident == activeCostId activeCost -> do
      let card = case activeCostTarget activeCost of ForCard c -> Just c; _ -> Nothing
          payer = activeCostIdentityId activeCost
          proposed = map (\(p, c) -> (Just p, c)) approved
      payers <- allianceCostPayers payer card
      case remaining of
        [] -> do
          affordable <- canAffordContributionPlan activeCost payers proposed
          if affordable
            then do
              paid (ApprovedAlliancePayment approved)
              for_ approved (\(who, c) -> contributeCost ident who c)
              paidCosts ident
            else abortPayment ident
        cost@(ChooseCharacterWithTraitCost _ _) : rest ->
          prepareAllianceCosts ident (approved <> [(payer, cost)]) rest
        ChosenHandCardCost : rest ->
          prepareAllianceCosts ident (approved <> [(payer, ChosenHandCardCost)]) rest
        cost : rest -> do
          eligible <- filterM (\p -> canAffordContributionPlan activeCost payers
            (proposed <> [(Just p, cost)] <> map (Nothing,) rest)) payers
          case eligible of
            [] -> abortPayment ident
            [p] -> offerAllianceCost ident p cost approved rest
            _ -> chooseOnePromptedAbout payer "$label.chooseCostContributor" (activeCostSource activeCost)
              [targeting p (offerAllianceCost ident p cost approved rest) | p <- eligible]
      pure activeCost
    OfferAllianceCost ident contributor cost approved remaining | ident == activeCostId activeCost -> do
      let accept = prepareAllianceCosts ident (approved <> [(contributor, cost)]) remaining
      if contributor == activeCostIdentityId activeCost
        then accept
        else chooseOnePromptedAbout contributor "$label.offerAllianceContribution" (activeCostSource activeCost)
          [ labeled "$label.contributeToCost" accept
          , labeled "$label.declineContribution" (abortPayment ident)
          ]
      pure activeCost
    ContributeCost ident contributor cost | ident == activeCostId activeCost -> do
      allowed <- allianceCostPayers (activeCostIdentityId activeCost) $ case activeCostTarget activeCost of
        ForCard card -> Just card
        _ -> Nothing
      when (contributor `elem` allowed) $
        payCostPartAs True (activeCost {activeCostIdentityId = contributor}) cost
      pure activeCost
    PayCost ident cost | ident == activeCostId activeCost -> do
      payCostPart activeCost cost
      pure activeCost
    SpentWithResources ident discard resources printed | ident == activeCostId activeCost -> do
      paid $ GeneratedByPayment (fromMaybe (activeCostIdentityId activeCost) $ pcController discard <|> pcOwner discard) <> PrintedResourcesPayment printed <> mconcat (map ResourcePayment resources)
      pure activeCost {activeCostSpentCards = discard : activeCostSpentCards activeCost}
    Spent ident discard | ident == activeCostId activeCost -> do
      resources <- resourcesFor discard $ case activeCostTarget activeCost of
        ForCard card -> Just card
        _ -> Nothing
      printed <- printedResourcesFor discard
      paid $ GeneratedByPayment (fromMaybe (activeCostIdentityId activeCost) $ pcController discard <|> pcOwner discard) <> PrintedResourcesPayment printed <> mconcat (map ResourcePayment resources)
      pure $
        activeCost
          { activeCostSpentCards = discard : activeCostSpentCards activeCost
          }
    Paid payment -> do
      let def = case activeCostTarget activeCost of ForCard card -> Just (getCardDef card); _ -> Nothing
      expanded <- for (payments payment) $ \receipt -> (receipt,) <$> case receipt of
        ResourcePayment r -> pure $ mconcat $ map ResourcePayment (doubleWildResources def [r])
        p@(ResourcePaymentFromCard _) -> mconcat . map ResourcePayment . doubleWildResources def <$> paymentResources p
        p -> pure p
      payment' <- declarable (normalizeSyncRatioExpansions expanded)
      generated <- paymentResources payment'
      let generator = fromMaybe (activeCostIdentityId activeCost) $ headMay [who | GeneratedByPayment who <- payments payment']
      unless (null generated) $ checkWindows [Window After $ GeneratedResources generator (fromIntegral $ length generated)]
      pure $
        activeCost
          { activeCostPayment = activeCostPayment activeCost <> payment'
          }
    DeclaredWild n r -> pure $ activeCost {activeCostPayment = declareWild n r (activeCostPayment activeCost)}
    CheckPayment ident | ident == activeCostId activeCost -> do
      let resources = resourceCosts (activeCostCost activeCost)
      countersPaid <- identityCounterCostsAvailable activeCost
      handTarget <- chosenHandCardAvailable activeCost
      if resources /= NoCost || not countersPaid || not handTarget
        then do
          -- A cost reduced to zero can still need a resource ability's ammo effect.
          payCostPart activeCost (if resources == NoCost then ResourceCost Nothing else resources)
          checkPayment ident
        else finishPayment ident
      pure activeCost
    -- A minion selected as a cost must actually enter play. Unique rejection is
    -- reported before payment completes; track its id so an unrelated unique
    -- rejection caused by that minion's entry/quickstrike cannot abort this cost.
    KeptOutByUnique (MinionRef mid)
      | mid `elem` [m | SearchedMinionPayment m <- payments (activeCostPayment activeCost)] -> do
          abortPayment (activeCostId activeCost)
          pure activeCost
    WithDiscarded (ActiveCostRef ident) _ cards
      | ident == activeCostId activeCost -> do
          for_ cards (paid . DiscardHandCardPayment)
          pure activeCost
    WithDiscardedMatch (ActiveCostRef ident) _ (EncounterCard card)
      | ident == activeCostId activeCost -> do
          -- The encounter card's whole reveal (including any questions it opens)
          -- is put ahead of PaidCosts, so the event's attack follows the cost.
          if cdCardType (getCardDef card) == TreacheryType
            then revealEncounterCard (activeCostIdentityId activeCost) card
            else putEncounterCardIntoPlay (activeCostIdentityId activeCost) card
          pure activeCost
    CardSpecific (ActiveCostRef acid) "discardTotalCost" (fromJSON -> Success (matcher, remaining :: Natural))
      | acid == activeCostId activeCost -> do
          hand <- projectMap PlayerIdentityHand unHand (activeCostIdentityId activeCost)
          let candidates = filter (\c -> cardMatch matcher c && not (staysInHand c) && activeCostSource activeCost /= toSource c) hand
              value = fromIntegral . fromMaybe 0 . cdCost . getCardDef
          if sum (map value candidates) < remaining then abortPayment acid
          else chooseOrRunOne (activeCostIdentityId activeCost) $
            [labeled "$label.done" (pure ()) | remaining == 0]
              <> [targeting c do
                    discardCard c
                    paid (DiscardHandCardPayment $ PlayerCard c)
                    cardSpecific (ActiveCostRef acid) "discardTotalCost" (matcher, subtractNatural (value c) remaining)
                 | c <- candidates]
          pure activeCost
    CardSpecific (ActiveCostRef acid) "assignIndirectCost" (fromJSON -> Success (remaining, assigned))
      | acid == activeCostId activeCost -> do
          candidates <- indirectCostCandidates (activeCostIdentityId activeCost) (activeCostSource activeCost)
          let used c = sum [n | (c', n) <- assigned, c == c']
              open = [(c, room - used c) | (c, room) <- candidates, room > used c]
          if remaining == 0 then do
            let allocations = [(c, used c) | c <- nub (map fst assigned)]
            before <- for allocations \(c, n) -> (c,,n) <$> characterStoredDamage c
            for_ allocations \(c, n) -> damageCharacter c (activeCostSource activeCost) (toDamage n FromAbility)
            cardSpecific (ActiveCostRef acid) "verifyIndirectCost" before
          else if sum (map snd open) < remaining then abortPayment acid
          else chooseOrRunOne (activeCostIdentityId activeCost)
            [targeting (characterRef c) $ cardSpecific (ActiveCostRef acid) "assignIndirectCost" (remaining - 1, (c, 1 :: Natural) : assigned) | (c, _) <- open]
          pure activeCost
    CardSpecific (ActiveCostRef acid) "verifyIndirectCost" (fromJSON -> Success before)
      | acid == activeCostId activeCost -> do
          complete <- and <$> for before (\(c, old, n) -> (>= old + n) <$> characterStoredDamage c)
          if complete then paid (DamagePayment $ sum [n | (_, _, n) <- before]) else abortPayment acid
          pure activeCost
    AbortPayment ident | ident == activeCostId activeCost -> do
      cancelMatchingMessage (== CheckPayment ident)
      let payer = activeCostIdentityId activeCost
      let reserved = paymentReservedAllyCards (activeCostPayment activeCost)
          reservedSupports = paymentReservedSupportCards (activeCostPayment activeCost)
      for_ reserved \(aid, card) -> do
        alive <- selectAny (AllyWithId aid)
        if alive then attachPlayableEventToAlly aid card else discardCard card
      for_ reservedSupports \(sid, card) -> do
        alive <- supportIsInPlay sid
        if alive then supportStep sid (AttachPlayableEvent card) else discardCard card
      -- a card played from a discard pile, or held by an upgrade, was left there
      fromHand <- case activeCostTarget activeCost of
        ForCard card -> do
          held <- getHeldPlayableCards payer
          aside <- getSetAsidePlayerCards
          -- PayForCard removes hand cards while payment is active. A card held,
          -- discarded, or set aside has not left its original zone yet.
          inDeck <- projectP PlayerIdentityDeck (elem card . unDeck) payer
          ((card `notElem` (map snd reserved <> map snd reservedSupports) && card `notElem` (held <> aside) && not inDeck) &&) <$> projectP PlayerIdentityDiscard (notElem card . unDiscard) payer
        _ -> pure False
      for_ [card | ForCard card <- [activeCostTarget activeCost], fromHand] (addToHand payer)
      for_ (activeCostSpentCards activeCost) \card -> addToHand (fromMaybe payer $ pcController card <|> pcOwner card) card
      disableActiveCost ident
      pure activeCost
    -- Only the final modified cost is actually spent; generation past that is
    -- wasted. Choose between distinct satisfying allocations before committing
    -- any non-resource costs (typed and different-resource costs still hold).
    FinishedPayment ident | ident == activeCostId activeCost -> do
      cancelMatchingMessage (== CheckPayment ident)
      case paymentActualResources (activeCostPayment activeCost) of
        Nothing -> do
          generated <- paymentResources (activeCostPayment activeCost)
          let allocations = actualResourceAllocations (activeCostCost activeCost) generated
          case allocations of
            [] -> abortPayment ident
            [resources] -> allocatePayment ident resources
            _ -> chooseOne (activeCostIdentityId activeCost)
              [labeled (allocationLabel resources) (allocatePayment ident resources) | resources <- allocations]
        Just resources
          | syncRatioResources (activeCostPayment activeCost) > 0
          , null [() | SyncRatioSpentPayment _ <- payments (activeCostPayment activeCost)] ->
              -- A save made after choosing typed resources but before provenance
              -- was recorded still gets the same explicit source allocation.
              allocatePayment ident resources
        Just _ -> do
          let card = case activeCostTarget activeCost of ForCard c -> Just c; _ -> Nothing
          payers <- allianceCostPayers (activeCostIdentityId activeCost) card
          if length payers > 1
            then prepareAllianceCosts ident [] (flattenCosts $ otherCosts $ activeCostCost activeCost)
            else do
              payCostPart activeCost (otherCosts $ activeCostCost activeCost)
              paidCosts ident
      pure activeCost
    AllocatedPayment ident resources | ident == activeCostId activeCost -> do
      generated <- paymentResources (activeCostPayment activeCost)
      if resources `elem` actualResourceAllocations (activeCostCost activeCost) generated
        then if syncRatioResources (activeCostPayment activeCost) == 0
          then do
            finishPayment ident
            pure activeCost {activeCostPayment = activeCostPayment activeCost <> ActualResourcesPayment resources}
          else case syncRatioSpendOptions resources (activeCostPayment activeCost) of
            [] -> abortPayment ident >> pure activeCost
            [n] -> do
              finishPayment ident
              pure activeCost {activeCostPayment = activeCostPayment activeCost <> ActualResourcesPayment resources <> SyncRatioSpentPayment n}
            counts -> do
              chooseOne (activeCostIdentityId activeCost)
                [ labeled (syncRatioAllocationLabel n) $
                    cardSpecific (ActiveCostRef ident) "allocateSyncRatio" (resources, n)
                | n <- counts ]
              pure activeCost
        else abortPayment ident >> pure activeCost
    CardSpecific (ActiveCostRef ident) "allocateSyncRatio" (fromJSON -> Success (resources, n :: Natural))
      | ident == activeCostId activeCost -> do
          generated <- paymentResources (activeCostPayment activeCost)
          if resources `elem` actualResourceAllocations (activeCostCost activeCost) generated
              && n `elem` syncRatioSpendOptions resources (activeCostPayment activeCost)
            then do
              finishPayment ident
              pure activeCost {activeCostPayment = activeCostPayment activeCost <> ActualResourcesPayment resources <> SyncRatioSpentPayment n}
            else abortPayment ident >> pure activeCost
    -- the cards spent are discarded and the cost done with, then what was paid for
    PaidCosts ident | ident == activeCostId activeCost -> do
      x <- spentX activeCost
      let
        payer = activeCostIdentityId activeCost
        payment = activeCostPayment activeCost <> x
      for_ (reverse $ activeCostSpentCards activeCost) discardCard
      -- "after you spend this card" (Pym Particles), before what was paid for resolves
      for_ (reverse $ activeCostSpentCards activeCost) \card ->
        afterSpent payer (fromMaybe payer $ pcController card <|> pcOwner card) (activeCostSpentCards activeCost) card
      disableActiveCost ident
      case activeCostTarget activeCost of
        ForCard card -> do
          -- one played from a discard pile, or held by an upgrade, is still there
          held <- getHeldPlayableCards payer
          inDiscard <- projectP PlayerIdentityDiscard (elem card . unDiscard) payer
          cardPlayed payer card payment (activeCostWindow activeCost)
          aside <- getSetAsidePlayerCards
          inDeck <- projectP PlayerIdentityDeck (elem card . unDeck) payer
          when (card `notElem` (map snd (paymentReservedAllyCards payment) <> map snd (paymentReservedSupportCards payment)) && card `notElem` (held <> aside) && not inDiscard && not inDeck) $
            checkWindows [Window After $ PlayedCardFromHand payer card]
        ForAbility ab -> do
          usedAbility payer ab
          windows <- case activeCostAbilityWindows activeCost of
            Just snapshot -> pure snapshot
            Nothing -> do
              occurrence <- getCurrentWindows
              triggering <- case abilityWindow ab of
                Nothing -> pure occurrence
                Just matcher -> filterM (\window -> windowMatches matcher window (abilitySource ab)) occurrence
              pure $ map windowType triggering
          for_ (abilityChoices ab) (resolveAs payer . abilityChoiceWithPayment payer windows payment)
          abilityResolved (abilitySource ab)
          -- an identity's printed ability, not its basic powers or changing form ("after
          -- you resolve Adam Warlock's Battle Mage")
          def <- getsGame \g -> case abilitySource ab of
            IdentityRef _ | abilityType ab == Basic || abilityIndex ab == 100 -> Nothing
            source -> abilityCardDef g (refocus source)
          -- whatever the ability does, the window after it resolves follows
          for_ def \d -> checkWindows [Window After $ ResolvedAbility payer d]
        ForTreachery -> pure ()
        ForContinuation _ continuation -> enqueueContinuation continuation
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
  countersPaid <- identityCounterCostsAvailable ActiveCost {..}
  handTarget <- chosenHandCardAvailable ActiveCost {..}
  resourcesPaid <- flip evalStateT prs $ do
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
  let requiredPrinted = printedResourceRequirements activeCostCost
      printedReceipts = [rs | PrintedResourcesPayment rs <- payments activeCostPayment]
      printedPaid = coversPrintedResources requiredPrinted (if null printedReceipts then concatMap (printedResources . getCardDef) activeCostSpentCards else concat printedReceipts)
        && coversPrintedResources requiredPrinted prs
  pure $ countersPaid && handTarget && resourcesPaid && printedPaid

-- | A cost's final payment includes derived values such as X. Choice expansion runs
-- before the handler's returned ActiveCost is installed, so pass that final payment
-- explicitly rather than asking RunAbility to read the previous game snapshot.
abilityChoiceWithPayment :: IdentityId -> [WindowType] -> Payment -> Choice -> Choice
abilityChoiceWithPayment who windows payment = \case
  Pay receipt -> Pay (GeneratedByPayment who <> receipt)
  RunAbility target index -> Run [RanAbility who target index windows payment, ClearRemoved]
  Label label choices -> Label label (map (abilityChoiceWithPayment who windows payment) choices)
  TargetLabel target choices -> TargetLabel target (map (abilityChoiceWithPayment who windows payment) choices)
  CardLabel code choice -> CardLabel code (abilityChoiceWithPayment who windows payment choice)
  ChooseOneLabelChoice choices -> ChooseOneLabelChoice [(label, abilityChoiceWithPayment who windows payment choice) | (label, choice) <- choices]
  choice -> choice

-- | Costs involving identity counters are actually payable before finishing the
-- resource stage; future generation is used only to decide whether play may start.
identityCounterCostsAvailable :: (HasGame m) => ActiveCost -> m Bool
identityCounterCostsAvailable activeCost = do
  current <- project PlayerIdentityCounters (activeCostIdentityId activeCost)
  pure $ current >= sum [n | IdentityCountersCost n <- flattenCosts (activeCostCost activeCost)]

-- | Choosing two traits cannot select the same physical character twice. Offer
-- only candidates that leave a way to complete the remaining choose-only costs.
validCharacterChoices :: (HasGame m) => ActiveCost -> Trait -> CharacterMatcher -> m [CharacterId]
validCharacterChoices activeCost trait matcher = do
  choices <- chooseCharacterTraitCandidates trait matcher
  future <- traverse (uncurry chooseCharacterTraitCandidates) remaining
  let used = paymentChosenCharacters (activeCostPayment activeCost)
      assign _ [] = True
      assign seen (xs : rest) = any (\x -> x `notElem` seen && assign (x : seen) rest) xs
  pure [c | c <- choices, c `notElem` used, assign (c : used) future]
 where
  required = [(t, m) | ChooseCharacterWithTraitCost t m <- flattenCosts (activeCostCost activeCost)]
  remaining = drop (1 + length (paymentChosenCharacters $ activeCostPayment activeCost)) required

chosenHandCardAvailable :: (HasGame m) => ActiveCost -> m Bool
chosenHandCardAvailable activeCost
  | ChosenHandCardCost `notElem` flattenCosts (activeCostCost activeCost) = pure True
  | otherwise = case paymentTarget (activeCostPayment activeCost) of
      Just (CardIdRef cid) -> projectP PlayerIdentityHand (any ((== cid) . pcCardId) . unHand) (activeCostIdentityId activeCost)
      _ -> pure False

-- | Count-vector allocations, so generation order cannot decide which icons were
-- spent and repeated identical icons do not produce duplicate choices.
actualResourceAllocations :: Cost -> [Resource] -> [[Resource]]
actualResourceAllocations cost generated
  | any uncapped parts = [generated]
  | otherwise = filter satisfies [allocation | target <- targets, allocation <- choose target [Physical, Mental, Energy, Wild]]
 where
  parts = flattenCosts cost
  requirements = costResources cost
  bounded = [fromIntegral n | UpToResourcesCost n <- parts]
  base = length requirements - length bounded
  targets = if null bounded then [length requirements]
    else reverse [length requirements .. min (length generated) (base + sum bounded)]
  uncapped = \case
    DynamicResourceCost _ -> True
    MayOverpay -> True
    _ -> False
  choose n [] = [[] | n == 0]
  choose n (r : rs) =
    [replicate k r <> rest | k <- [0 .. min n (length $ filter (== r) generated)], rest <- choose (n - k) rs]
  satisfies resources = coversResources requirements resources
    && coversDifferentTypes (differentResourcesNeeded cost) resources

-- | Choosing between identical resource types from distinct payment sources.
syncRatioAllocationLabel :: Natural -> Text
syncRatioAllocationLabel = countLabel "spendSyncRatioResources"

allocationLabel :: [Resource] -> Text
allocationLabel resources = ikeyWith "label.spendResourceAllocation"
  [intVar name (length $ filter (== r) resources) | (name, r) <- [("physical", Physical), ("mental", Mental), ("energy", Energy), ("wild", Wild)]]

-- | Keep the initiating player's card context while another player contributes.
validEventExhaustions :: (HasGame m) => ActiveCost -> [CharacterId] -> m Bool
validEventExhaustions cost characters = case activeCostTarget cost of
  ForCard card -> do
    payer <- fromMaybe (activeCostIdentityId cost) <$> getActiveCostPayer (activeCostId cost)
    eventExhaustionsValid payer card characters
  _ -> pure True
