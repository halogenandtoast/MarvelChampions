module Marvel.Cost (
  module Marvel.Cost,
  module Marvel.Cost.Types,
) where

import Marvel.Prelude

import Data.List qualified as L
import Marvel.Ability
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
passesCanAffordCost identityId a = go (abilityCost a)
 where
  go = \case
    NoCost -> pure True
    DamageCost _ -> pure True
    HealCost n -> case source of
      IdentityRef ident ->
        member ident
          <$> select (IdentityWithDamage $ AtLeast $ Static $ fromIntegral n)
      _ -> error "Unhandled"
    DamageThisCost _ -> pure True
    DiscardCost _ -> pure True -- TODO: we need to check if target exists
    DiscardHandCardCost n ->
      if n < 1
        then pure True
        else projectP PlayerIdentityHand (notNull . unHand) identityId
    ExhaustCost -> case source of
      IdentityRef ident -> member ident <$> select UnexhaustedIdentity
      AllyRef ident -> member ident <$> select UnexhaustedAlly
      SupportRef ident -> member ident <$> select UnexhaustedSupport
      UpgradeRef ident -> member ident <$> select UnexhaustedUpgrade
      _ -> error "Unhandled"
    UseCost -> case source of
      UpgradeRef ident -> member ident <$> select UpgradeWithAnyUses
      SupportRef ident -> member ident <$> select SupportWithAnyUses
      AllyRef ident -> member ident <$> select AllyWithAnyUses
      _ -> error "Unhandled"
    ResourceCost mr -> do
      resources <- getAvailableResourcesFor Nothing
      pure $ case mr of
        Nothing -> not (null resources)
        Just r -> r `elem` resources || Wild `elem` resources
    DynamicResourceCost mr -> do
      resources <- getAvailableResourcesFor Nothing
      pure $ case mr of
        Nothing -> not (null resources)
        Just r -> r `elem` resources || Wild `elem` resources
    MultiResourceCost mr -> do
      resources <- getAvailableResourcesFor Nothing
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
    Costs xs -> allM go xs
  source = abilitySource a
