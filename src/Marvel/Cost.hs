module Marvel.Cost (
  module Marvel.Cost,
  module Marvel.Cost.Types,
) where

import Marvel.Prelude

import Marvel.Card.Def (cardMatch, CardMatcher (CardWithType), CardType (MinionType, SideSchemeType))

import Marvel.Modifier.Types (Modifier (CannotBeDiscarded))

import Data.List qualified as L
import Marvel.Ability
import Marvel.Card.PlayerCard ()
import Marvel.Card.PlayerCard.Types (PlayerCard (..), staysInHand)
import Marvel.Cost.Types
import Marvel.Game.Source
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
passesCanAffordCost identityId a = canAffordCost identityId (abilitySource a) =<< getAbilityCost a

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
canAffordCost identityId source = go
 where
  go = \case
    NoCost -> pure True
    -- a tough status card would prevent the damage, so it cannot be paid (RR FAQ, Focused
    -- Rage)
    DamageCost _ -> projectP PlayerIdentityTough not identityId
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
    DiscardCost target -> notElem CannotBeDiscarded <$> getModifiers target
    DiscardRandomHandCardsCost n ->
      projectP PlayerIdentityHand ((>= n) . fromIntegral . length . unHand) identityId
    DiscardHandCardsCost n _ ->
      projectP PlayerIdentityHand ((>= n) . fromIntegral . length . filter (\c -> notPlaying c && not (staysInHand c)) . unHand) identityId
    ExhaustUpgradeCost matcher -> selectAny (matcher <> UnexhaustedUpgrade <> upgradeControlledBy identityId)
    ReturnUpgradeToHandCost matcher -> selectAny (matcher <> upgradeControlledBy identityId)
    DiscardUpgradeCost matcher -> selectAny (matcher <> UpgradeThatCanBeDiscarded <> upgradeControlledBy identityId)
    DiscardAllyCost matcher -> selectAny (matcher <> AllyControlledBy (IdentityWithId identityId))
    DiscardTopOfDeckCost -> projectP PlayerIdentityDeck (not . null . unDeck) identityId
    -- "up to": as few as the deck holds, down to the least the cost asks
    DiscardTopOfDeckCardsCost lo _ -> projectP PlayerIdentityDeck ((>= lo) . fromIntegral . length . unDeck) identityId
    DiscardTopOfDeckForDamageCost -> do
      n <- pendingDamageAmount identityId
      projectP PlayerIdentityDeck ((>= n) . fromIntegral . length . unDeck) identityId
    ExhaustAlliesCost n _ ->
      (>= n) . fromIntegral . length
        <$> selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId identityId))
    CountersFromThisCost n -> (>= n) <$> getCountersOn source
    ResourcesFromHandCost c ->
      coversResources (costResources c) <$> (concatMapM (`resourcesFor` Nothing) =<< getAvailablePaymentSources identityId)
    ExhaustAlliesMatchingCost matcher n ->
      (>= n) <$> selectListCount (matcher <> UnexhaustedAlly <> AllyControlledBy (IdentityWithId identityId))
    ExhaustCharacterWithTraitCost t ->
      orM
        [ selectAny (IdentityWithId identityId <> UnexhaustedIdentity <> IdentityWithTrait t)
        , selectAny (UnexhaustedAlly <> AllyControlledBy (IdentityWithId identityId) <> AllyWithTrait t)
        ]
    DiscardEncounterCardsUntilMinionCost -> pure True
    SearchEncounterSideSchemeCost -> any (cardMatch $ CardWithType SideSchemeType) <$> getEncounterDeck
    SearchEncounterMinionCost -> any (cardMatch $ CardWithType MinionType) <$> ((<>) <$> getEncounterDeck <*> getEncounterDiscard)
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
      AttachmentRef _ -> member identityId <$> select (UnexhaustedIdentity <> HeroIdentity)
      -- "exhaust your alter-ego" on an expert campaign's obligation
      ObligationRef _ -> member identityId <$> select (UnexhaustedIdentity <> AlterEgoIdentity)
      -- "exhaust your hero" on a main scheme (The Grand Collection)
      MainSchemeRef _ -> member identityId <$> select (UnexhaustedIdentity <> HeroIdentity)
      CardIdRef _ -> member identityId <$> select UnexhaustedIdentity
      _ -> error "Unhandled"
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
    Costs xs -> allM go xs
  notPlaying c = source /= CardIdRef (pcCardId c)
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
