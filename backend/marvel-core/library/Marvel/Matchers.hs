{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Matchers
  ( module Marvel.Matchers
  , module Marvel.Matchers.Types
  ) where

import Marvel.Prelude

import {-# SOURCE #-} Marvel.Card.EncounterCard
import Marvel.Damage
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers.Types

identityMatches :: MonadGame env m => IdentityMatcher -> IdentityId -> m Bool
identityMatches matcher ident = member ident <$> gameSelectIdentity matcher

allyMatches :: MonadGame env m => AllyMatcher -> AllyId -> m Bool
allyMatches matcher ident = member ident <$> gameSelectAlly matcher

upgradeMatches :: MonadGame env m => UpgradeMatcher -> UpgradeId -> m Bool
upgradeMatches matcher ident = member ident <$> gameSelectUpgrade matcher

enemyMatches :: MonadGame env m => EnemyMatcher -> EnemyId -> m Bool
enemyMatches matcher ident = member ident <$> gameSelectEnemy matcher

villainMatches :: MonadGame env m => VillainMatcher -> VillainId -> m Bool
villainMatches matcher ident = member ident <$> gameSelectVillain matcher

treacheryMatches
  :: MonadGame env m => TreacheryMatcher -> TreacheryId -> m Bool
treacheryMatches matcher ident = member ident <$> gameSelectTreachery matcher

schemeMatches :: MonadGame env m => SchemeMatcher -> SchemeId -> m Bool
schemeMatches matcher ident = member ident <$> gameSelectScheme matcher

sideSchemeMatches
  :: MonadGame env m => SideSchemeMatcher -> SideSchemeId -> m Bool
sideSchemeMatches matcher ident = member ident <$> gameSelectSideScheme matcher

minionMatches :: MonadGame env m => MinionMatcher -> MinionId -> m Bool
minionMatches matcher ident = member ident <$> gameSelectMinion matcher

gameValueMatches :: MonadGame env m => GameValueMatcher -> Natural -> m Bool
gameValueMatches matcher n = case matcher of
  AnyValue -> pure True
  GreaterThan v -> do
    value <- fromIntegral <$> fromGameValue v
    pure $ n > value
  AtLeast v -> do
    value <- fromIntegral <$> fromGameValue v
    pure $ n >= value

characterMatches
  :: MonadGame env m => CharacterMatcher -> CharacterId -> m Bool
characterMatches matcher ident = member ident <$> gameSelectCharacter matcher

encounterCardMatches :: BasicEncounterCardMatcher -> EncounterCard -> Bool
encounterCardMatches matcher _ = case matcher of
  AnyEncounterCard -> True

damageMatches :: MonadGame env m => DamageMatcher -> Damage -> m Bool
damageMatches matcher damage = case matcher of
  AttackFromPlayer identityMatcher -> case damageSource damage of
    FromPlayerAttack ident -> identityMatches identityMatcher ident
    _ -> pure False
  AttackFromAlly allyMatcher -> case damageSource damage of
    FromAllyAttack ident -> allyMatches allyMatcher ident
    _ -> pure False
  AnyDamage -> pure True

instance Count IdentityMatcher where
  data QueryCount IdentityMatcher = SustainedDamage | HeroAttackDamage
  selectCount = gameSelectCountIdentity

instance Count SchemeMatcher where
  data QueryCount SchemeMatcher = SchemeThreat
  selectCount = gameSelectCountScheme

