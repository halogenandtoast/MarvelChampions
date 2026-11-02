{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.ActiveCost (
  module Marvel.ActiveCost,
  module Marvel.ActiveCost.Types,
) where

import Marvel.Prelude
import Control.Monad (replicateM_)
import System.Random.Shuffle (shuffleM)
import Marvel.Modifier.Types (Modifier (CannotBeDiscarded, CannotLeavePlayByCardAbilities, CannotTakeDamage, CannotTakeDamageFromPlayerCards, CannotHaveAttachedPlayerCards, ContinuouslyRecalculateResourceCost, AdditionalReadyCost))

import Control.Monad.State
import Data.List (partition, nub, nubBy)
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
import Marvel.GameValue.Types (GameValue (Static))
import Marvel.I18n (ikeyWith, intVar)
import Marvel.Id (CharacterId (..), IdentityId, MinionId (..), SchemeId (..), SideSchemeId (..))
import Marvel.Identity.Types
import Marvel.Message.Lifted
import Marvel.Payment
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Matchers qualified as Matchers
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Queue (HasQueue)
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait (Trait)
import Marvel.Resource.Resources (afterSpent)
import Marvel.Resource.Resources.AvianAnatomy (avianAnatomySpent)
import Marvel.Resource.Resources.MolecularAcceleration (molecularAccelerationSpent)
import Marvel.Resource.Resources.PassionForJustice (passionForJusticeSpent)
import Marvel.Resource.Resources.MutantGenesis (mutantEnergySpent)
import Marvel.Resource.Resources.Shield (specialFundingSpent)
import Marvel.Resource.Resources.Cyclops (cyclopsResourceSpent)
import Marvel.Window (windowMatches)
import Marvel.Window.Types
    ( Window(windowType, Window),
      WindowTiming(After),
      WindowType(PlayedCardFromHand, IdentityTookDamage,
                 GeneratedResources) )


-- | The card receiving resources includes the source of a paid card ability.
activeCostPaymentCard :: HasGame m => ActiveCost -> m (Maybe PlayerCard)
activeCostPaymentCard cost = case activeCostTarget cost of
  ForCard card -> pure $ Just card
  ForAbility _ -> resourcePaymentCard (activeCostIdentityId cost) (activeCostSource cost)
  _ -> pure Nothing

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
    PlaceIdentityCountersCost n -> placeCounters iid n
    DiscardHandCardsCost n m -> do
      inHand <- projectMap PlayerIdentityHand (fromIntegral . length . unHand) iid
      discardFor iid (ActiveCostRef acid) FromHand n (fromMaybe inHand m)
    DiscardHandCardsWithTotalCost matcher n -> cardSpecific (ActiveCostRef acid) "discardTotalCost" (matcher, n)
    ExactCountersCost n -> do
      count <- getCountersOn (activeCostSource activeCost)
      if count < n then abortPayment acid else removeCounters (activeCostSource activeCost) n >> paid (CountersPayment n)
    DiscardToughCost n -> do
      tough <- project PlayerIdentityTough iid
      extra <- project PlayerIdentityAdditionalTough iid
      if (if tough then 1 else 0) + extra < n
        then abortPayment acid
        else replicateM_ (fromIntegral n) (removeStatus iid ToughStatus)
    RemoveEnemyStatusCost enemy status -> removeStatus enemy status
    GiveEnemyToughCost enemy -> do
      tough <- characterMatches (CharacterWithStatus ToughStatus) (toCharacter enemy)
      if tough then abortPayment acid else enemyBecomesTough enemy
    EnemyAttacksCost enemy -> do
      enemyAttacks enemy iid
      paid NoPayment
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
    TouchedDamageCost n -> do
      candidates <- selectList (CharacterOtherThan $ IdentityCharacter iid)
      host <- touchedHost iid
      valid <- filterM (fmap (\ms -> CannotHaveAttachedPlayerCards `notElem` ms) . getModifiers . characterRef) (filter (\c -> Just c /= host) candidates)
      available <- canFindTouched iid
      if null valid || not available then abortPayment acid else chooseOne iid [targeting (characterRef c) do
        findAndAttachTouched iid c
        damageCharacter c (activeCostSource activeCost) (toDamage n FromAbility)
        paid (TouchedPayment c [] <> DamagePayment n)
        | c <- valid]
    DamageCharacterCost matcher n -> do
      candidates <- selectList matcher
      valid <- filterM (fmap (\ms -> CannotTakeDamage `notElem` ms && CannotTakeDamageFromPlayerCards `notElem` ms) . getModifiers . characterRef) candidates
      chooseOne iid [targeting (characterRef c) (damageCharacter c (activeCostSource activeCost) (toDamage n FromAbility) >> paid (ChosenCharacterPayment c <> DamagePayment n)) | c <- valid]
    ExhaustAnyIdentityCost -> do
      identities <- selectList UnexhaustedIdentity
      chooseOne iid [targeting who (exhaust who >> paid (ExhaustedIdentityPayment who)) | who <- identities]
    MainSchemeAccelerationTokenCost -> do
      schemes <- selectList EncounterMainScheme
      chooseOrRunOne iid [targeting scheme (placeAccelerationToken scheme >> paid MainSchemeAccelerationTokenPayment) | SchemeMainSchemeId scheme <- schemes]
    DamageCost n -> do
      paid (PendingDamageCostPayment n 0)
      damageIdentity iid (activeCostSource activeCost) (toDamage n FromAbility)
      cardSpecific (ActiveCostRef acid) "verifyDamageCost" n
    UpToRemainingHitPointsDamageCost -> do
      (remaining, _, _) <- getRemainingHitPoints iid
      affordable <- canAffordCost iid (activeCostSource activeCost) UpToRemainingHitPointsDamageCost
      if not affordable then abortPayment acid
      else chooseOne iid
        [ labeled (countLabel "takeDamage" n) (payCost acid $ DamageCost n)
        | n <- [1 .. remaining]
        ]
    HealCost n -> do
      heal iid n
      paid $ HealPayment n
    DealDamageThisCost n -> case activeCostTarget activeCost of
      ForAbility a -> case abilitySource a of
        AllyRef ally -> do
          damageCharacter ally (abilitySource a) (toDamage n FromAbility)
          paid $ DamageThisPayment n
        _ -> error "DealDamageThisCost requires an ally source"
      _ -> error "DealDamageThisCost requires an ability"
    DamageThisCost n -> case activeCostTarget activeCost of
      ForAbility a -> case abilitySource a of
        AllyRef ally -> do
          damageCharacter ally (abilitySource a) (toDamage n FromAbility)
          paid $ DamageThisPayment n
        _ -> error "Unhandled"
      _ -> error "Unhandled"
    ExhaustAllIdentitiesAndDamageCost n -> do
      players <- getAllPlayers
      for_ players $ \p -> do
        exhaust p
        damageIdentity p (activeCostSource activeCost) (toDamage n FromAbility)
      paid ExhaustPayment
    ExhaustAttachmentCost -> do
      case activeCostTarget activeCost of
        ForAbility a | AttachmentRef aid <- abilitySource a -> attachmentStep aid ExhaustedAttachment
        _ -> pure ()
      paid ExhaustPayment
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
    PutSideSchemeFromVictoryCost -> do
      encounter <- map EncounterCard <$> getVictoryDisplay
      player <- map PlayerCard <$> getPlayerVictoryDisplay
      let schemes = [card | card <- encounter <> player, cdCardType (getCardDef card) `elem` [SideSchemeType, PlayerSideSchemeType]]
      if null schemes then abortPayment acid else chooseOne iid
        [targeting (CardIdRef $ toCardId card) $ do
          putVictorySideSchemeIntoPlay iid card
          paid $ TargetPayment $ SideSchemeRef $ SideSchemeId $ unCardId $ toCardId card
        | card <- schemes]
    RemoveFromGameCost target -> do
      removeFromGame target
      paid (DiscardPayment target)
    ShuffleDiscardCardIntoDeckCost matcher -> do
      cards <- filter (cardMatch matcher) <$> projectMap PlayerIdentityDiscard unDiscard iid
      if null cards then abortPayment acid else chooseOne iid
        [targeting card (shuffleIntoDeck iid [card]) | card <- cards]
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
    CountersFromUpgradeCost matcher n -> do
      upgrades <- selectList (matcher <> upgradeControlledBy iid <> UpgradeWithUses (AtLeast $ Static $ fromIntegral n))
      oneOf upgrades (\u -> removeCounters u n >> paid (CountersPayment n))
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
    ReturnAnyAllyToHandCost matcher -> do
      let required = [m | ReturnAnyAllyToHandCost m <- flattenCosts (activeCostCost activeCost)]
          paidCount = length [() | ReturnedAllyPayment _ <- payments (activeCostPayment activeCost)]
          remaining = drop (paidCount + 1) required
      candidates <- selectList (matcher <> AllyThatCanLeavePlay)
      allies <- filterM (\aid -> do
        slots <- for remaining $ \m -> map AllyRef . filter (/= aid) <$> selectList (m <> AllyThatCanLeavePlay)
        pure $ distinctExhaustions slots) candidates
      if null allies then abortPayment acid else oneOf allies $ \aid -> do
        card <- getsGame (\g -> cardInPlay g (AllyRef aid))
        returnToHand aid
        for_ [c | Just (PlayerCard c) <- [card]] $ \c -> paid (ReturnedAllyPayment c)
    ReturnAllyToHandCost matcher -> do
      allies <- selectList (matcher <> AllyThatCanLeavePlay <> AllyControlledBy (IdentityWithId iid))
      oneOf allies returnToHand
    ReturnUpgradeToHandCost matcher -> do
      upgrades <- selectList (matcher <> upgradeControlledBy iid)
      oneOf upgrades returnToHand
    DiscardEnemyUpgradeCost matcher hostMatcher -> do
      upgrades <- selectList (UpgradeWithCard matcher <> UpgradeThatCanBeDiscarded)
      attached <- filterM (\uid -> getUpgradeAttachedEnemy uid >>= maybe (pure False) (\enemy -> asYou iid $ enemyMatches hostMatcher enemy)) upgrades
      if null attached then abortPayment acid else oneOf attached \u -> do
        enemy <- getUpgradeAttachedEnemy u
        card <- getsGame (\g -> cardInPlay g (UpgradeRef u))
        for_ ((,) <$> enemy <*> card) $ \(host, physicalCard) -> paid (DiscardedUpgradeFromEnemyPayment host physicalCard)
        discardUpgrade u
        paid (DiscardPayment $ toTarget u)
    DiscardUpgradeCost matcher -> do
      upgrades <- selectList (matcher <> UpgradeThatCanBeDiscarded <> upgradeControlledBy iid)
      oneOf upgrades \u -> do
        card <- getsGame (\g -> cardInPlay g (UpgradeRef u))
        for_ card (paid . DiscardedUpgradePayment)
        discardUpgrade u
        paid (DiscardPayment $ toTarget u)
    DiscardAllyCost matcher -> do
      allies <- selectList (matcher <> AllyThatCanLeavePlay <> AllyControlledBy (IdentityWithId iid))
      oneOf allies \aid -> do
        card <- getsGame $ \g -> cardInPlay g (AllyRef aid)
        for_ card (paid . DiscardedAllyPayment)
        (hp, thw, atk) <- getAllyPrintedStats aid
        paid $ DiscardedAllyStatsPayment hp thw atk
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
    DiscardTopOfDeckForSupportsCost -> do
      n <- fromIntegral . length <$> selectList (SupportControlledBy $ IdentityWithId iid)
      when (n > 0) $ discardFromDeck iid n (ActiveCostRef acid)
    DiscardTopOfDeckForDamageCost -> do
      n <- pendingDamageAmount iid
      when (n > 0) $ discardFromDeck iid n (ActiveCostRef acid)
    -- one at a time, until the player is done or has exhausted as many as it allows
    ExhaustUpgradesMatchingCost matcher minimumCount maximumCount -> do
      upgrades <- selectList $ matcher <> UnexhaustedUpgrade <> UpgradeControlledBy (IdentityWithId iid)
      if length upgrades < fromIntegral minimumCount then abortPayment acid
      else when (maximumCount > 0 && notNull upgrades) $ chooseOne iid $
        [labeled "$label.done" (pure ()) | minimumCount == 0]
        <> [targeting uid (exhaust uid >> paid (ExhaustedCardPayment $ toTarget uid) >> payCost acid (ExhaustUpgradesMatchingCost matcher (subtractNatural 1 minimumCount) (maximumCount - 1))) | uid <- upgrades]
    ExhaustCharactersWithThwartGreaterThanCost enemy ->
      cardSpecific (ActiveCostRef acid) "fwChooseThwartExhaustion" (enemy, [] :: [CharacterId])
    ExhaustControlledCardsCost n -> do
      cards <- readyControlledCardCandidates iid
      if length cards < fromIntegral n
        then abortPayment acid
        else when (n > 0) $ chooseOne iid
          [targeting target do
             case target of
               IdentityRef p -> exhaust p
               AllyRef a -> exhaust a
               SupportRef s -> exhaust s
               UpgradeRef u -> exhaust u
               SideSchemeRef s -> sideSchemeStep s ExhaustedSideScheme
               _ -> pure ()
             paid (ExhaustedCardPayment target)
             payCost acid (ExhaustControlledCardsCost (n - 1))
          | target <- cards]
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
    ExhaustAlliesWithTraitCost trait n m -> do
      allies <- selectList (AllyWithTrait trait <> UnexhaustedAlly <> AllyControlledBy (IdentityWithId iid))
      when (maybe True (> 0) m && notNull allies) $
        chooseOnePromptedAbout iid "$label.chooseAllyToExhaust" (activeCostSource activeCost) $
          [labeled "$label.done" (pure ()) | n == 0]
            <> [targeting a do
                  exhaust a
                  paid (ExhaustedAllyPayment a)
                  payCost acid (ExhaustAlliesWithTraitCost trait (subtractNatural 1 n) (subtractNatural 1 <$> m))
               | a <- allies]
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
    ResolveSettingSpecialCost -> do
      cardSpecific GameRef "aoaPaySettingSpecial" (iid, acid)
      paid UsePayment
    ReadyAllyCost matcher -> do
      candidates <- selectList (matcher <> Matchers.ExhaustedAlly <> AllyCanReady <> AllyControlledBy (IdentityWithId iid))
      allies <- filterM (\ally -> do
        extra <- mconcat . mapMaybe (\case AdditionalReadyCost c -> Just c; _ -> Nothing) <$> getModifiers ally
        canAffordCost iid (toSource ally) extra) candidates
      if null allies then abortPayment acid else chooseOne iid
        [ targeting ally do
            extra <- mconcat . mapMaybe (\case AdditionalReadyCost c -> Just c; _ -> Nothing) <$> getModifiers ally
            payCost acid extra
            cardSpecific ally "readyWithCostPaid" ()
            paid (ReadiedAllyPayment ally)
        | ally <- allies
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
    RevealNemesisMinionCost -> do
      minions <- getNemesisMinionsOutOfPlay iid
      if null minions then abortPayment acid else chooseOne iid
        [targeting card $ do
          searchEverywhereAndRevealEncounterCard iid (CardIs $ getCardDef card)
          paid $ SearchedMinionPayment $ MinionId $ unCardId $ ecCardId card
        | card <- minions]
    SearchEncounterMinionCost -> do
      deck <- getEncounterDeck
      discard <- getEncounterDiscard
      let found = filter (cardMatch $ CardWithType MinionType) (deck <> discard)
      if null found then abortPayment acid else
        chooseOneSearchedIn iid "$label.searchEncounterDeck" [("$label.encounterDeck", map EncounterCard deck), ("$label.encounterDiscard", map EncounterCard discard)]
          [targeting card (paid (SearchedMinionPayment $ MinionId $ unCardId $ ecCardId card) >> putEncounterCardIntoPlay iid card >> shuffleEncounterDeck) | card <- found]
    PlaceThreatOnMainSchemeCost n -> traverse_ (`placeThreat` n) =<< selectList MainScheme
    DealEncounterCardToPlayerCost matcher -> do
      players <- selectList matcher
      if null players then abortPayment acid else chooseOne iid
        [targeting (IdentityRef player) (dealEncounterCard player >> paid (TargetPayment $ IdentityRef player)) | player <- players]
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
    LookAtTopEncounterCardsAndDiscardOneCost n -> do
      cards <- take (fromIntegral n) <$> getEncounterDeck
      if null cards then abortPayment acid else do
        showCardsTo iid (map EncounterCard cards)
        chooseOne iid
          [ targeting (CardIdRef $ ecCardId card) do
              hideCardsFrom iid
              putOnTopOfEncounterDeck [card]
              discardTopOfEncounterDeck 1
              paid (DiscardedEncounterCardPayment $ EncounterCard card)
          | card <- cards
          ]
    -- added to the resources as the card was played
    TargetPrintedCost -> pure ()
    ChooseEnemyCost matcher -> do
      enemies <- selectList matcher
      if null enemies then abortPayment acid else chooseOne iid [targeting (enemyRef enemy) (paid $ TargetPayment $ enemyRef enemy) | enemy <- enemies]
    DiscardMatchingHandCardCost matcher -> do
      hand <- projectMap PlayerIdentityHand unHand iid
      let candidates = filter (\c -> cardMatch matcher c && not (staysInHand c)) hand
      if null candidates then abortPayment acid else chooseOne iid [targeting c (discardCard c >> paid (DiscardHandCardPayment $ PlayerCard c)) | c <- candidates]
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
    SameResourcesCost _ -> resourcePayment iid contributing
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
  discardable target = (\mods -> not (any (`elem` mods) [CannotBeDiscarded, CannotLeavePlayByCardAbilities])) <$> getModifiers target
  oneOf :: (ReverseQueue n, IsRef a) => [a] -> (a -> n ()) -> n ()
  oneOf xs f = case xs of
    [x] -> f x
    [] -> pure ()
    _ -> chooseOne iid [targeting x (f x) | x <- xs]
  -- only what can be spent on it is offered: "you can only spend [physical] resources"
  resourcePayment payer contributor = do
    mcard <- activeCostPaymentCard activeCost
    let
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
  ( HasGame m
  , HasQueue m
  , MonadThrow m
  ) =>
  Payment ->
  m Payment
declarable = fmap mconcat . traverse one . payments
 where
  one = \case
    ResourcePayment Wild -> pure $ WildPayment Wild
    p@(ResourcePaymentFromCard _) -> mconcat . map asPaid <$> paymentResources p
    p@(ResourcePaymentFromPlayerCard _) -> mconcat . map asPaid <$> paymentResources p
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
  ForPaymentContinuation source _ _ _ -> source

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
isResourceCost (SameResourcesCost _) = True
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
  ( HasGame m
  , HasQueue m
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
  (MonadThrow m, MonadRandom m, HasGame m, HasQueue m, Projection m PlayerIdentity) =>
  Message ->
  ActiveCost ->
  m ActiveCost
runActiveCost msg activeCost = do
  current <- if refreshStep then refreshContinuousCardCost activeCost else pure activeCost
  runActiveCostWithCurrentCost msg current
 where
  acid = activeCostId activeCost
  refreshStep = case msg of
    CheckPayment ident -> ident == acid
    FinishedPayment ident -> ident == acid
    AllocatedPayment ident _ -> ident == acid
    CardSpecific (ActiveCostRef ident) key _ -> ident == acid && key `elem` ["finishSpendingCard", "allocateSyncRatio"]
    _ -> False

-- Ordinary cards lock in their cost before resource payment. A continuous
-- cost-setting ability explicitly opts in to reevaluation while paying.
refreshContinuousCardCost :: (HasGame m, MonadThrow m, Projection m PlayerIdentity) => ActiveCost -> m ActiveCost
refreshContinuousCardCost activeCost = case activeCostTarget activeCost of
  ForCard card -> do
    continuous <- elem ContinuouslyRecalculateResourceCost <$> getModifiers card
    if not continuous then pure activeCost else do
      cost <- recalculateCardPaymentCost (activeCostIdentityId activeCost) card (activeCostPayment activeCost)
      pure activeCost {activeCostCost = cost}
  _ -> pure activeCost

runActiveCostWithCurrentCost ::
  (MonadThrow m, MonadRandom m, HasGame m, HasQueue m) =>
  Message -> ActiveCost -> m ActiveCost
runActiveCostWithCurrentCost msg activeCost = runQueueT $ case msg of
    CheckWindows windows -> do
      let actual = sum [damageAmount damage | Window After (IdentityTookDamage you source damage) <- windows, you == activeCostIdentityId activeCost, source == activeCostSource activeCost]
          receipt = \case
            PendingDamageCostPayment n taken -> PendingDamageCostPayment n (taken + actual)
            other -> other
      pure activeCost {activeCostPayment = mconcat $ map receipt $ payments $ activeCostPayment activeCost}
    CardSpecific (ActiveCostRef acid) "verifyDamageCost" (fromJSON -> Success (n :: Natural)) | acid == activeCostId activeCost -> do
      let receipts = payments (activeCostPayment activeCost)
          taken = sum [actual | PendingDamageCostPayment _ actual <- receipts]
          keep = \case PendingDamageCostPayment _ _ -> False; _ -> True
      if taken >= n then paid (DamagePayment n) else abortPayment acid
      pure activeCost {activeCostPayment = mconcat $ filter keep receipts}
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
    CardSpecific (ActiveCostRef ident) "fwChooseThwartExhaustion" (fromJSON -> Success (enemy, chosen :: [CharacterId])) | ident == activeCostId activeCost -> do
      candidates <- filter (`notElem` chosen) <$> readyCharacterCandidates (activeCostIdentityId activeCost) AnyCharacter
      total <- sum <$> traverse getCharacterThwart chosen
      required <- getEnemyScheme enemy
      chooseOne (activeCostIdentityId activeCost) $
        [labeled "$label.done" (for_ chosen (\character -> do
          case character of
            IdentityCharacter who -> exhaust who
            AllyCharacter aid -> exhaust aid
            _ -> pure ()
          paid $ ExhaustedCharacterPayment character)) | total > required]
        <> [targeting (characterRef character) $ cardSpecific (ActiveCostRef ident) "fwChooseThwartExhaustion" (enemy, chosen <> [character]) | character <- candidates]
      pure activeCost
    PayCost ident cost | ident == activeCostId activeCost -> do
      frameworkDiscard $ payCostPart activeCost cost
      pure activeCost
    SpentWithResources ident discard _ printed | ident == activeCostId activeCost, canonicalCardCode (getCardDef discard) == "50014" -> do
      cardSpecific (ActiveCostRef ident) "organizationalSupport" (discard,printed,[] :: [Target],[] :: [Resource])
      pure activeCost {activeCostSpentCards = discard : activeCostSpentCards activeCost}
    Spent ident discard | ident == activeCostId activeCost, canonicalCardCode (getCardDef discard) == "50014" -> do
      printed <- printedResourcesFor discard
      cardSpecific (ActiveCostRef ident) "organizationalSupport" (discard,printed,[] :: [Target],[] :: [Resource])
      pure activeCost {activeCostSpentCards = discard : activeCostSpentCards activeCost}
    CardSpecific (ActiveCostRef ident) "organizationalSupport" (fromJSON -> Success (card,printed :: [Resource],chosen :: [Target],icons :: [Resource])) | ident == activeCostId activeCost -> do
      let you = fromMaybe (activeCostIdentityId activeCost) (pcController card <|> pcOwner card)
      candidates <- filter ((`notElem` chosen) . fst) <$> organizationalSupportCandidates you
      chooseOne you $ labeled "$label.done" (cardSpecific (ActiveCostRef ident) "finishSpendingCard" (card,printedResources (getCardDef card) <> icons,printed)) :
        [ targeting target do
            case target of
              AllyRef aid -> exhaust aid
              SupportRef sid -> exhaust sid
              _ -> pure ()
            cardSpecific (ActiveCostRef ident) "organizationalSupport" (card,printed,chosen<>[target],icons<>resources)
        | length chosen < 3,(target,resources) <- candidates]
      pure activeCost
    SpentWithResources ident discard resources printed | ident == activeCostId activeCost -> do
      -- Reserve the card now; its interrupt resolves before any resource from it
      -- is recorded as spent. The continuation observes the resulting game.
      molecularAccelerationSpent (fromMaybe (activeCostIdentityId activeCost) $ pcController discard <|> pcOwner discard) discard
      cardSpecific (ActiveCostRef ident) "finishSpendingCard" (discard, resources, printed)
      pure activeCost {activeCostSpentCards = discard : activeCostSpentCards activeCost}
    Spent ident discard | ident == activeCostId activeCost -> do
      context <- activeCostPaymentCard activeCost
      resources <- resourcesFor discard context
      printed <- printedResourcesFor discard
      molecularAccelerationSpent (fromMaybe (activeCostIdentityId activeCost) $ pcController discard <|> pcOwner discard) discard
      cardSpecific (ActiveCostRef ident) "finishSpendingCard" (discard, resources, printed)
      pure activeCost {activeCostSpentCards = discard : activeCostSpentCards activeCost}
    CardSpecific (ActiveCostRef ident) "finishSpendingCard" (fromJSON -> Success (discard, resources, printed))
      | ident == activeCostId activeCost -> do
          paid $ GeneratedByPayment (fromMaybe (activeCostIdentityId activeCost) $ pcController discard <|> pcOwner discard) <> PrintedResourcesPayment printed <> mconcat (map ResourcePayment resources)
          pure activeCost
    Paid payment -> do
      def <- fmap getCardDef <$> activeCostPaymentCard activeCost
      expanded <- for (payments payment) $ \receipt -> (receipt,) <$> case receipt of
        ResourcePayment r -> pure $ mconcat $ map ResourcePayment (doubleWildResources def [r])
        p@(ResourcePaymentFromCard _) -> mconcat . map ResourcePayment . doubleWildResources def <$> paymentResources p
        p@(ResourcePaymentFromPlayerCard _) -> mconcat . map ResourcePayment . doubleWildResources def <$> paymentResources p
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
          frameworkDiscard $ payCostPart activeCost (if resources == NoCost then ResourceCost Nothing else resources)
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
          reservedTop = paymentReservedTopDeckCards (activeCostPayment activeCost)
      for_ reserved \(aid, card) -> do
        alive <- selectAny (AllyWithId aid)
        if alive then attachPlayableEventToAlly aid card else discardCard card
      for_ reservedSupports \(sid, card) -> do
        alive <- supportIsInPlay sid
        if alive then supportStep sid (AttachPlayableEvent card) else discardCard card
      for_ reservedTop $ cardSpecific payer "restoreTopDeckPlay"
      -- a card played from a discard pile, or held by an upgrade, was left there
      fromHand <- case activeCostTarget activeCost of
        ForCard card -> do
          held <- getHeldPlayableCards payer
          aside <- getSetAsidePlayerCards
          -- PayForCard removes hand cards while payment is active. A card held,
          -- discarded, or set aside has not left its original zone yet.
          inDeck <- projectP PlayerIdentityDeck (elem card . unDeck) payer
          ((card `notElem` (reservedTop <> map snd reserved <> map snd reservedSupports) && card `notElem` (held <> aside) && not inDeck) &&) <$> projectP PlayerIdentityDiscard (notElem card . unDiscard) payer
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
              frameworkDiscard $ payCostPart activeCost (otherCosts $ activeCostCost activeCost)
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
        handSpent = reverse $ activeCostSpentCards activeCost
        receipted = paymentSpentCardsWithPayers payer (activeCostPayment activeCost)
        -- Hand cards retain their original contributor. A tucked card's explicit
        -- receipt records the player who spent it, while ownership stays intact.
        spent = nubBy (\(_, a) (_, b) -> pcCardId a == pcCardId b) $
          [(fromMaybe payer $ pcController card <|> pcOwner card, card) | card <- handSpent] <> receipted
        cards = map snd spent
        alreadyReceipted card = any ((== pcCardId card) . pcCardId . snd) receipted
        payment = activeCostPayment activeCost <> x <> Payments [SpentCardPayment card | card <- handSpent, not $ alreadyReceipted card]
      -- Held-card abilities already discarded their own card. Discard hand cards
      -- here once; do not discard receipted held cards a second time.
      for_ handSpent discardCard
      -- Interrupts to spending resolve before responses to having spent a card.
      case activeCostTarget activeCost of
        ForCard played -> for_ spent \(who, card) -> cyclopsResourceSpent who played card
        _ -> pure ()
      -- "after you spend this card" includes resources spent as if from hand.
      for_ spent \(who, card) -> afterSpent payer who cards card
      case activeCostTarget activeCost of
        ForCard played -> for_ spent \(who, card) -> do
          mutantEnergySpent who played card
          passionForJusticeSpent who played card
          specialFundingSpent who played card
          avianAnatomySpent who played card
        _ -> pure ()
      disableActiveCost ident
      case activeCostTarget activeCost of
        ForCard card -> do
          -- one played from a discard pile, or held by an upgrade, is still there
          held <- getHeldPlayableCards payer
          inDiscard <- projectP PlayerIdentityDiscard (elem card . unDiscard) payer
          cardPlayed payer card payment (activeCostWindow activeCost)
          aside <- getSetAsidePlayerCards
          inDeck <- projectP PlayerIdentityDeck (elem card . unDeck) payer
          medLabPlay <- anyM (\(sid, reserved) -> do
            medLab <- elem sid <$> selectList (SupportNamed "Med Lab")
            pure $ reserved == card && medLab) (paymentReservedSupportCards payment)
          when (card `elem` paymentReservedTopDeckCards payment || medLabPlay || (card `notElem` (map snd (paymentReservedAllyCards payment) <> map snd (paymentReservedSupportCards payment)) && card `notElem` (held <> aside) && not inDiscard && not inDeck)) $
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
          -- A labeled card ability begins only after its costs are paid. The game
          -- applies status cancellation and defense declaration before its effects.
          cardSpecific GameRef "resolveLabeledAbility" (payer, ab, windows, payment)
        ForTreachery -> pure ()
        ForContinuation _ continuation -> enqueueContinuation continuation
        ForPaymentContinuation _ target key value -> cardSpecific target key (value, payment)
      pure $ activeCost {activeCostPayment = payment}
    _ -> pure activeCost

resourceCostPaid ::
  ( HasGame m
  , HasQueue m
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
        && coversSameType (sameResourcesNeeded activeCostCost) prs
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
    && coversSameType (sameResourcesNeeded cost) resources

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
