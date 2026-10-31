module Marvel.Cost (
  module Marvel.Cost,
  module Marvel.Cost.Types,
) where

import Marvel.Prelude

import Marvel.Card.Def (printedResources, getCardDef, cdCost, cdKeywords, cardMatch, CardMatcher (CardWithType), CardType (MinionType, SideSchemeType))

import Marvel.Modifier.Types (Modifier (CannotBeDiscarded, CannotLeavePlay))
import Marvel.Keyword (Keyword (Permanent))

import Data.List qualified as L
import Marvel.Ability
import Marvel.Card.PlayerCard (printedResourcesFor)
import Marvel.Card.PlayerCard.Types (PlayerCard (..), staysInHand)
import Marvel.Cost.Types
import Marvel.Game.Source
import Marvel.Message (characterRef)
import Marvel.GameValue
import Marvel.Deck
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Projection
import Marvel.Query
import Marvel.Queue
import Marvel.Modifier.Types (Modifier (BasicAttackAdditionalCost))
import Marvel.Question qualified as Q
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait (Trait)
import Marvel.Status (StatusCard (ToughStatus))
import Marvel.Modifier.Types (Modifier (CannotTakeDamage, CannotTakeDamageFromPlayerCards, DamageReduction))

passesCanAffordCost ::
  ( HasQueue m
  , MonadRandom m
  , MonadThrow m
  , Projection m PlayerIdentity
  , HasGame m
  ) =>
  IdentityId ->
  Ability ->
  m Bool
passesCanAffordCost identityId a = asPayingForAbility a $ canAffordCost identityId (abilitySource a) =<< getAbilityCost a

-- | Whether the resources cover those asked for, each of a type or of any ('Nothing'):
-- a wild resource covers any type.
coversResources :: [Maybe Resource] -> [Resource] -> Bool
coversResources mr resources = null uncovered && length remaining >= length anyResources
 where
  goPay [] bs = ([], bs)
  goPay as [] = (as, [])
  goPay (a' : as) bs = case (a' `elem` bs, Wild `elem` bs) of
    (True, _) -> goPay as (L.delete a' bs)
    (_, True) -> goPay as (L.delete Wild bs)
    (False, False) -> (a' : as, bs)
  (anyResources, specificResources) = partitionMaybes mr
  (uncovered, remaining) = goPay specificResources resources

-- | The ability's cost with what modifiers add to it: "as an additional cost for each
-- hero to make a basic attack" (Mother's Orders).
getAbilityCost :: (HasGame m) => Ability -> m Cost
getAbilityCost a = case abilitySource a of
  IdentityRef ident | abilityType a == Basic, Q.Attack `elem` abilityChoices a -> do
    modifiers <- getModifiers ident
    pure $ abilityCost a <> mconcat [c | BasicAttackAdditionalCost c <- modifiers]
  _ -> pure $ abilityCost a

-- | Whether the player can pay the cost, besides its resources when it is a card's: a
-- card being played is not in hand to be discarded for its own cost.
canAffordCost ::
  ( HasQueue m
  , MonadRandom m
  , MonadThrow m
  , Projection m PlayerIdentity
  , HasGame m
  ) =>
  IdentityId ->
  Source ->
  Cost ->
  m Bool
canAffordCost identityId source cost = do
  ordinary <- go cost
  let required = printedResourceRequirements cost
  if null required then pure ordinary else do
    printed <- concatMapM printedResourcesFor . filter notPlaying =<< getAvailablePaymentSources identityId
    pure $ ordinary && coversPrintedResources required printed
 where
  go = \case
    NoCost -> pure True
    -- a tough status card would prevent the damage, so it cannot be paid (RR FAQ, Focused
    -- Rage)
    IndirectDamageCost n -> (>= n) . sum . map snd <$> indirectCostCandidates identityId source
    DamageCharacterCost matcher _ -> do
      candidates <- selectList matcher
      notNull <$> filterM (fmap (\ms -> CannotTakeDamage `notElem` ms && CannotTakeDamageFromPlayerCards `notElem` ms) . getModifiers . characterRef) candidates
    DamageCost n -> do
      tough <- project PlayerIdentityTough identityId
      modifiers <- getModifiers (IdentityRef identityId)
      let immune = CannotTakeDamage `elem` modifiers || CannotTakeDamageFromPlayerCards `elem` modifiers
          reduced = sum [k | DamageReduction k <- modifiers]
      pure $ n == 0 || not (tough || immune || reduced > 0)
    HealCost n ->
      member identityId
        <$> select (IdentityWithDamage $ AtLeast $ Static $ fromIntegral n)
    DamageThisCost n -> case source of
      AllyRef aid -> do
        present <- selectAny (AllyWithId aid)
        tough <- match (AllyCharacter aid) (CharacterWithStatus ToughStatus)
        modifiers <- getModifiers source
        let immune = CannotTakeDamage `elem` modifiers || CannotTakeDamageFromPlayerCards `elem` modifiers
            reduced = sum [k | DamageReduction k <- modifiers]
        -- Paying with the last hit point is legal: this cost deals damage, rather
        -- than spending hit points. Tough or a prevention effect cannot pay it.
        pure $ present && (n == 0 || not (tough || immune || reduced > 0))
      _ -> pure False
    RemoveFromGameCost target -> do
      present <- getsGame (maybe False (notElem Permanent . cdKeywords . getCardDef) . (\g -> cardInPlay g target))
      canLeave <- notElem CannotLeavePlay <$> getModifiers target
      pure $ present && canLeave
    DiscardControlledCardCost matcher -> notNull <$> discardableControlledCardsMatching identityId matcher
    DiscardCost target -> notElem CannotBeDiscarded <$> getModifiers target
    DiscardHandCardsWithTotalCost matcher n ->
      projectP PlayerIdentityHand ((>= n) . sum . map (fromIntegral . fromMaybe 0 . cdCost . getCardDef) . filter (\c -> notPlaying c && not (staysInHand c) && cardMatch matcher c) . unHand) identityId
    ExactCountersCost n -> (>= n) <$> getCountersOn source
    DiscardToughCost n -> do
      tough <- project PlayerIdentityTough identityId
      extra <- project PlayerIdentityAdditionalTough identityId
      pure $ (if tough then 1 else 0) + extra >= n
    GiveEnemyToughCost enemy -> not <$> characterMatches (CharacterWithStatus ToughStatus) (toCharacter enemy)
    GiveEnemyBoostCost _ _ -> pure True
    DiscardRandomHandCardsCost n ->
      projectP PlayerIdentityHand ((>= n) . fromIntegral . length . unHand) identityId
    DiscardRandomHandCardsMatchingCost n matcher ->
      projectP PlayerIdentityHand ((>= n) . fromIntegral . length . filter (\c -> notPlaying c && not (staysInHand c) && cardMatch matcher c) . unHand) identityId
    DiscardHandCardsCost n _ ->
      projectP PlayerIdentityHand ((>= n) . fromIntegral . length . filter (\c -> notPlaying c && not (staysInHand c)) . unHand) identityId
    ExhaustSupportCost matcher -> selectAny (SupportMatches [matcher, UnexhaustedSupport, SupportControlledBy (IdentityWithId identityId)])
    ExhaustUpgradeCost matcher -> selectAny (matcher <> UnexhaustedUpgrade <> upgradeControlledBy identityId)
    ReturnUpgradeToHandCost matcher -> selectAny (matcher <> upgradeControlledBy identityId)
    DiscardUpgradeCost matcher -> selectAny (matcher <> UpgradeThatCanBeDiscarded <> upgradeControlledBy identityId)
    DiscardAllyCost matcher -> selectAny (matcher <> AllyControlledBy (IdentityWithId identityId))
    DiscardSupportCost matcher -> notNull <$> discardableSupports matcher
    DiscardAllyOrSupportCost allies supports -> do
      ownAllies <- filterM discardable =<< selectList (allies <> AllyControlledBy (IdentityWithId identityId))
      ownSupports <- discardableSupports supports
      pure $ notNull ownAllies || notNull ownSupports
    DiscardTopOfDeckCost -> projectP PlayerIdentityDeck (not . null . unDeck) identityId
    -- "up to": as few as the deck holds, down to the least the cost asks
    DiscardTopOfDeckCardsCost lo _ -> projectP PlayerIdentityDeck ((>= lo) . fromIntegral . length . unDeck) identityId
    DiscardTopOfDeckForDamageCost -> do
      n <- pendingDamageAmount identityId
      projectP PlayerIdentityDeck ((>= n) . fromIntegral . length . unDeck) identityId
    ReturnAllyToHandCost matcher -> selectAny (matcher <> AllyControlledBy (IdentityWithId identityId))
    ExhaustAlliesWithTraitCost trait n _ ->
      (>= n) <$> selectListCount (AllyWithTrait trait <> UnexhaustedAlly <> AllyControlledBy (IdentityWithId identityId))
    ExhaustAlliesCost n _ ->
      (>= n) . fromIntegral . length
        <$> selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId identityId))
    CountersFromThisCost n -> (>= n) <$> getCountersOn source
    ResourcesFromHandCost c ->
      coversResources (costResources c) <$> (concatMapM (`resourcesFor` Nothing) =<< getAvailablePaymentSources identityId)
    ExhaustAlliesMatchingCost matcher n ->
      (>= n) <$> selectListCount (matcher <> UnexhaustedAlly <> AllyControlledBy (IdentityWithId identityId))
    ExhaustCharacterCost matcher -> notNull <$> readyCharacterCandidates identityId matcher
    ExhaustCharacterWithTraitCost t ->
      orM
        [ selectAny (IdentityWithId identityId <> UnexhaustedIdentity <> IdentityWithTrait t)
        , selectAny (UnexhaustedAlly <> AllyControlledBy (IdentityWithId identityId) <> AllyWithTrait t)
        ]
    ExhaustCardsWithTraitCost t lo hi ->
      if lo > hi then pure False else (>= lo) . fromIntegral . length <$> readyTraitCardCandidates identityId t
    DiscardEncounterCardsUntilMinionCost -> pure True
    -- The encounter deck is hidden. Whether the required treachery was found is
    -- checked as this cost resolves; failing to reveal one cannot pay the cost.
    RevealEncounterTreacheryCost -> pure True
    ShuffleEncounterCardsIntoOwnDeckCost n -> (>= n) . fromIntegral . length <$> ((<>) <$> getEncounterDeck <*> getEncounterDiscard)
    SearchEncounterSideSchemeCost -> any (cardMatch $ CardWithType SideSchemeType) <$> getEncounterDeck
    SearchEncounterMinionCost -> any (cardMatch $ CardWithType MinionType) <$> ((<>) <$> getEncounterDeck <*> getEncounterDiscard)
    PlaceThreatOnMainSchemeCost _ -> selectAny MainScheme
    DealEncounterCardCost -> pure True
    DiscardTopEncounterCardCost -> pure True
    TargetPrintedCost -> pure True
    ChosenHandCardCost -> pure True
    ChooseCharacterWithTraitCost t matcher -> notNull <$> chooseCharacterTraitCandidates t matcher
    MayOverpay -> pure True
    PrintedXCost _ _ -> pure True
    UpToResourcesCost n -> (n > 0 &&) . notNull <$> availableResources
    IdentityCountersCost n -> member identityId <$> select (IdentityWithCounters $ AtLeast $ Static $ fromIntegral n)
    ExhaustMilanoCost -> milanoIsReady
    IfControlsMilanoCost c c' -> controlsMilano identityId >>= \m -> go (if m then c else c')
    ExhaustCost -> case source of
      IdentityRef ident -> member ident <$> select UnexhaustedIdentity
      AllyRef ident -> member ident <$> select UnexhaustedAlly
      SupportRef ident -> member ident <$> select UnexhaustedSupport
      UpgradeRef ident -> member ident <$> select UnexhaustedUpgrade
      -- "exhaust your hero" on an encounter card
      AttachmentRef _ -> member identityId <$> select UnexhaustedIdentity
      -- "exhaust your alter-ego" on an expert campaign's obligation
      ObligationRef _ -> member identityId <$> select (UnexhaustedIdentity <> AlterEgoIdentity)
      -- "exhaust your hero" on a main scheme (The Grand Collection)
      MainSchemeRef _ -> member identityId <$> select (UnexhaustedIdentity <> HeroIdentity)
      CardIdRef _ -> member identityId <$> select UnexhaustedIdentity
      _ -> error "Unhandled"
    CountersFromUpgradeCost matcher n -> selectAny (matcher <> UpgradeControlledBy (IdentityWithId identityId) <> UpgradeWithUses (AtLeast $ Static $ fromIntegral n))
    UseCost -> case source of
      UpgradeRef ident -> member ident <$> select UpgradeWithAnyUses
      SupportRef ident -> member ident <$> select SupportWithAnyUses
      AllyRef ident -> member ident <$> select AllyWithAnyUses
      _ -> error "Unhandled"
    ResourceCost mr -> do
      resources <- availableResources
      pure $ case mr of
        Nothing -> not (null resources)
        Just r -> r `elem` resources || Wild `elem` resources
    DynamicResourceCost mr -> do
      resources <- availableResources
      pure $ case mr of
        Nothing -> not (null resources)
        Just r -> r `elem` resources || Wild `elem` resources
    MultiResourceCost mr -> coversResources mr <$> availableResources
    PrintedResourceCost rs -> coversPrintedResources rs <$> (concatMapM printedResourcesFor . filter notPlaying =<< getAvailablePaymentSources identityId)
    DifferentResourcesCost n ->
      coversDifferentTypes n <$> getAvailableResourcesFor identityId Nothing
    -- the Milano exhausted to pay, its own resource is not there to spend as well
    Costs xs | ExhaustMilanoCost `elem` xs -> do
      milano <- getMilano
      abilities <- getResourceAbilities identityId Nothing
      resources <- getAvailableResourcesFor identityId Nothing
      let milanos = length [a | a <- abilities, Just (abilitySource a) == (SupportRef <$> milano)]
          others = foldr L.delete resources (replicate milanos Wild)
      andM
        [ milanoIsReady
        , allM go [x | x <- xs, null (costResources x), x /= ExhaustMilanoCost]
        , pure $ coversResources (concatMap costResources xs) others
        ]
    -- the deck must have a card for each of its top cards discarded
    Costs xs
      | let discards = length (filter (== DiscardTopOfDeckCost) xs)
      , discards > 1 ->
          andM
            [ projectP PlayerIdentityDeck ((>= discards) . length . unDeck) identityId
            , allM go (filter (/= DiscardTopOfDeckCost) xs)
            ]
    Costs xs | any isTraitCardExhaustion (flattenCost xs) -> do
      let parts = flattenCost xs
      slots <- concatMapM exhaustionSlots parts
      others <- allM go parts
      pure $ others && distinctExhaustions slots
    Costs xs -> allM go xs
  flattenCost = concatMap (\case Costs cs -> flattenCost cs; c -> [c])
  isTraitCardExhaustion = \case ExhaustCardsWithTraitCost _ _ _ -> True; ExhaustCharacterCost _ -> True; _ -> False
  exhaustionSlots = \case
    ExhaustCharacterCost matcher -> pure . pure . map characterRef =<< readyCharacterCandidates identityId matcher
    ExhaustCardsWithTraitCost t lo _ -> replicate (fromIntegral lo) <$> readyTraitCardCandidates identityId t
    ExhaustCharacterWithTraitCost t -> do
      candidates <- readyTraitCardCandidates identityId t
      pure [[r | r <- candidates, case r of IdentityRef _ -> True; AllyRef _ -> True; _ -> False]]
    ExhaustUpgradeCost matcher -> do
      candidates <- selectList (matcher <> UnexhaustedUpgrade <> upgradeControlledBy identityId)
      pure [map UpgradeRef candidates]
    ExhaustAlliesCost lo _ -> do
      candidates <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId identityId))
      pure $ replicate (fromIntegral lo) (map AllyRef candidates)
    ExhaustAlliesMatchingCost matcher n -> do
      candidates <- selectList (matcher <> UnexhaustedAlly <> AllyControlledBy (IdentityWithId identityId))
      pure $ replicate (fromIntegral n) (map AllyRef candidates)
    ExhaustCost -> pure [[case source of
      IdentityRef i -> IdentityRef i
      AllyRef a -> AllyRef a
      SupportRef s -> SupportRef s
      UpgradeRef u -> UpgradeRef u
      _ -> IdentityRef identityId]]
    _ -> pure []
  notPlaying c = source /= CardIdRef (pcCardId c)
  discardable target = notElem CannotBeDiscarded <$> getModifiers target
  discardableSupports matcher = filterM discardable =<< selectList (SupportMatches [matcher, SupportControlledBy (IdentityWithId identityId)])
  -- a card's ability used from its owner's hand (Pip the Troll) is not paid for with
  -- that card
  availableResources = case source of
    CardIdRef _ -> do
      abilities <- concatMapM abilityResources =<< getResourceAbilities identityId Nothing
      cards <- filter notPlaying <$> getAvailablePaymentSources identityId
      (<> abilities) <$> concatMapM (`resourcesFor` Nothing) cards
    _ -> getAvailableResourcesFor identityId Nothing

-- | Alliance may split costs among players. Character exhaustions are allocated
-- together so a dual-trait character cannot pay two exhaustions by itself.
canAffordAllianceCost ::
  (HasQueue m, MonadRandom m, MonadThrow m, Projection m PlayerIdentity, HasGame m) =>
  [IdentityId] -> Source -> Cost -> m Bool
canAffordAllianceCost payers source cost = do
  characters <- traverse candidates traitCosts
  chosen <- traverse (uncurry chooseCharacterTraitCandidates) chooseCosts
  let distinct _ [] = True
      distinct used (xs : rest) = any (\x -> x `notElem` used && distinct (x : used) rest) xs
  others <- allM (\c -> anyM (\p -> canAffordCost p source c) payers) otherCosts'
  pure $ distinct [] characters && distinct [] chosen && others
 where
  parts = flatten cost
  traitCosts = [t | ExhaustCharacterWithTraitCost t <- parts]
  chooseCosts = [(t, m) | ChooseCharacterWithTraitCost t m <- parts]
  otherCosts' = [c | c <- parts, case c of ExhaustCharacterWithTraitCost _ -> False; ChooseCharacterWithTraitCost _ _ -> False; _ -> True]
  flatten (Costs cs) = concatMap flatten cs
  flatten NoCost = []
  flatten c = [c]
  candidates t = concatMapM (\p -> do
    heroes <- selectList (IdentityWithId p <> UnexhaustedIdentity <> IdentityWithTrait t)
    allies <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId p) <> AllyWithTrait t)
    pure $ map IdentityCharacter heroes <> map AllyCharacter allies) payers

-- | A choose-only cost does not spend another player's game element, so it needs
-- no ownership contribution or consent. The character may already be exhausted.
chooseCharacterTraitCandidates :: (HasGame m) => Trait -> CharacterMatcher -> m [CharacterId]
chooseCharacterTraitCandidates trait matcher = selectList (CharacterWithTrait trait <> matcher)

-- | The payer's ready cards of a trait, with each card represented by its own ref.
-- A hero's traits are those of its face-up form, as for every other trait matcher.
readyTraitCardCandidates :: (HasGame m) => IdentityId -> Trait -> m [Target]
readyTraitCardCandidates ident trait = do
  heroes <- selectList (IdentityWithId ident <> UnexhaustedIdentity <> IdentityWithTrait trait)
  allies <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId ident) <> AllyWithTrait trait)
  supports <- selectList (SupportMatches [UnexhaustedSupport, SupportControlledBy (IdentityWithId ident), SupportWithTrait trait])
  upgrades <- selectList (UnexhaustedUpgrade <> upgradeControlledBy ident <> UpgradeWithTrait trait)
  pure $ map IdentityRef heroes <> map AllyRef allies <> map SupportRef supports <> map UpgradeRef upgrades

-- | Each exhaustion in a combined cost needs its own ready card, even when one card
-- has several of the requested traits.
distinctExhaustions :: [[Target]] -> Bool
distinctExhaustions = go []
 where
  go _ [] = True
  go spent (choices : rest) = any (\c -> c `notElem` spent && go (c : spent) rest) choices

-- | A cost may exhaust only a ready character controlled by its payer. Filtering
-- separately from the character matcher preserves form and other matching rules.
readyCharacterCandidates :: (HasGame m) => IdentityId -> CharacterMatcher -> m [CharacterId]
readyCharacterCandidates ident matcher = do
  identities <- selectList (IdentityWithId ident <> UnexhaustedIdentity)
  allies <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId ident))
  filterM (characterMatches matcher) (map IdentityCharacter identities <> map AllyCharacter allies)

-- | Unlike ordinary typed resource costs, a printed wild is not a printed mental,
-- energy or physical icon, even when the player would declare it as that type.
coversPrintedResources :: [Resource] -> [Resource] -> Bool
coversPrintedResources required icons = all (\r -> count (== r) required <= count (== r) icons) [Energy, Mental, Physical, Wild]
