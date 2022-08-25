module Marvel.Cost (module Marvel.Cost, module Marvel.Cost.Types) where

import Marvel.Prelude

import Data.List qualified as L
import Marvel.Cost.Types
import Marvel.Source
import Marvel.Ability
import Marvel.Entity
import Marvel.GameValue
import Marvel.Id
import Marvel.Resource
import Marvel.Query
import Marvel.Matchers
import Marvel.Game.Source

passesCanAffordCost :: MonadGame env m => IdentityId -> Ability -> m Bool
passesCanAffordCost identityId a = go (abilityCost a)
 where
  go = \case
    NoCost -> pure True
    DamageCost _ -> pure True
    HealCost n -> case source of
      IdentitySource ident -> member ident
        <$> select (IdentityWithDamage $ AtLeast $ Static $ fromIntegral n)
      _ -> error "Unhandled"
    DamageThisCost _ -> pure True
    DiscardHandCardCost n -> fieldP PlayerIdentityHand notNull identityId
    ExhaustCost -> case source of
      IdentitySource ident -> member ident <$> select UnexhaustedIdentity
      AllySource ident -> member ident <$> select UnexhaustedAlly
      SupportSource ident -> member ident <$> select UnexhaustedSupport
      UpgradeSource ident -> member ident <$> select UnexhaustedUpgrade
      _ -> error "Unhandled"
    UseCost -> case source of
      UpgradeSource ident -> member ident <$> select UpgradeWithAnyUses
      SupportSource ident -> member ident <$> select SupportWithAnyUses
      AllySource ident -> member ident <$> select AllyWithAnyUses
      _ -> error "Unhandled"
    ResourceCost mr -> do
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

