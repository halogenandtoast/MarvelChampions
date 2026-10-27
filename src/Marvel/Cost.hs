module Marvel.Cost (
  module Marvel.Cost,
  module Marvel.Cost.Types,
) where

import Marvel.Prelude

import Data.List qualified as L
import Marvel.Ability
import Marvel.Card.PlayerCard.Types (PlayerCard (..))
import Marvel.Cost.Types
import Marvel.Game.Source
import Marvel.GameValue
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
    DamageCost _ -> pure True
    HealCost n ->
      member identityId
        <$> select (IdentityWithDamage $ AtLeast $ Static $ fromIntegral n)
    DamageThisCost _ -> pure True
    DiscardCost _ -> pure True -- TODO: we need to check if target exists
    DiscardRandomHandCardsCost n ->
      projectP PlayerIdentityHand ((>= n) . fromIntegral . length . unHand) identityId
    DiscardHandCardsCost n _ ->
      projectP PlayerIdentityHand ((>= n) . fromIntegral . length . filter notPlaying . unHand) identityId
    ExhaustUpgradeCost matcher -> selectAny (matcher <> UnexhaustedUpgrade <> upgradeControlledBy identityId)
    ReturnUpgradeToHandCost matcher -> selectAny (matcher <> upgradeControlledBy identityId)
    ExhaustAlliesCost n _ ->
      (>= n) . fromIntegral . length
        <$> selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId identityId))
    ExhaustCharacterWithTraitCost t ->
      orM
        [ selectAny (IdentityWithId identityId <> UnexhaustedIdentity <> IdentityWithTrait t)
        , selectAny (UnexhaustedAlly <> AllyControlledBy (IdentityWithId identityId) <> AllyWithTrait t)
        ]
    DiscardEncounterCardsUntilMinionCost -> pure True
    DealEncounterCardCost -> pure True
    DiscardTopEncounterCardCost -> pure True
    TargetPrintedCost -> pure True
    MayOverpay -> pure True
    ExhaustCost -> case source of
      IdentityRef ident -> member ident <$> select UnexhaustedIdentity
      AllyRef ident -> member ident <$> select UnexhaustedAlly
      SupportRef ident -> member ident <$> select UnexhaustedSupport
      UpgradeRef ident -> member ident <$> select UnexhaustedUpgrade
      -- "exhaust your hero" on an encounter card
      AttachmentRef _ -> member identityId <$> select (UnexhaustedIdentity <> HeroIdentity)
      -- "exhaust your alter-ego" on an expert campaign's obligation
      ObligationRef _ -> member identityId <$> select (UnexhaustedIdentity <> AlterEgoIdentity)
      _ -> error "Unhandled"
    UseCost -> case source of
      UpgradeRef ident -> member ident <$> select UpgradeWithAnyUses
      SupportRef ident -> member ident <$> select SupportWithAnyUses
      AllyRef ident -> member ident <$> select AllyWithAnyUses
      _ -> error "Unhandled"
    ResourceCost mr -> do
      resources <- getAvailableResourcesFor identityId Nothing
      pure $ case mr of
        Nothing -> not (null resources)
        Just r -> r `elem` resources || Wild `elem` resources
    DynamicResourceCost mr -> do
      resources <- getAvailableResourcesFor identityId Nothing
      pure $ case mr of
        Nothing -> not (null resources)
        Just r -> r `elem` resources || Wild `elem` resources
    MultiResourceCost mr -> do
      resources <- getAvailableResourcesFor identityId Nothing
      let
        goPay [] bs = ([], bs)
        goPay as [] = (as, [])
        goPay (a' : as) bs = case (a' `elem` bs, Wild `elem` bs) of
          (True, _) -> goPay as (L.delete a' bs)
          (_, True) -> goPay as (L.delete Wild bs)
          (False, False) -> (a' : as, bs)
        (anyResources, specificResources) = partitionMaybes mr
        (uncovered, remaining) = goPay specificResources resources
      pure $ null uncovered && length remaining >= length anyResources
    DifferentResourcesCost n ->
      coversDifferentTypes n <$> getAvailableResourcesFor identityId Nothing
    Costs xs -> allM go xs
  notPlaying c = source /= CardIdRef (pcCardId c)
