{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Matchers (
  module Marvel.Matchers,
  module Marvel.Matchers.Types,
) where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card.Def
import Marvel.Card.EncounterCard
import Marvel.Count
import Marvel.Damage
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Ref (Ref (AllyRef, EventRef, IdentityRef), Source)

identityMatches :: (HasGame m) => IdentityMatcher -> IdentityId -> m Bool
identityMatches matcher ident = member ident <$> inAnyArea (gameSelectIdentity matcher)

allyMatches :: (HasGame m) => AllyMatcher -> AllyId -> m Bool
allyMatches matcher ident = member ident <$> inAnyArea (gameSelectAlly matcher)

upgradeMatches :: (HasGame m) => UpgradeMatcher -> UpgradeId -> m Bool
upgradeMatches matcher ident = member ident <$> inAnyArea (gameSelectUpgrade matcher)

enemyMatches :: (HasGame m) => EnemyMatcher -> EnemyId -> m Bool
enemyMatches matcher ident = member ident <$> inAnyArea (gameSelectEnemy matcher)

villainMatches :: (HasGame m) => VillainMatcher -> VillainId -> m Bool
villainMatches matcher ident = member ident <$> inAnyArea (gameSelectVillain matcher)

treacheryMatches ::
  (HasGame m) => TreacheryMatcher -> TreacheryId -> m Bool
treacheryMatches matcher ident = member ident <$> inAnyArea (gameSelectTreachery matcher)

schemeMatches :: (HasGame m) => SchemeMatcher -> SchemeId -> m Bool
schemeMatches matcher ident = member ident <$> inAnyArea (gameSelectScheme matcher)

sideSchemeMatches ::
  (HasGame m) => SideSchemeMatcher -> SideSchemeId -> m Bool
sideSchemeMatches matcher ident = member ident <$> inAnyArea (gameSelectSideScheme matcher)

minionMatches :: (HasGame m) => MinionMatcher -> MinionId -> m Bool
minionMatches matcher ident = member ident <$> inAnyArea (gameSelectMinion matcher)

gameValueMatches :: (HasGame m) => GameValueMatcher -> Natural -> m Bool
gameValueMatches matcher n = case matcher of
  AnyValue -> pure True
  GreaterThan v -> do
    value <- fromIntegral <$> fromGameValue v
    pure $ n > value
  AtLeast v -> do
    value <- fromIntegral <$> fromGameValue v
    pure $ n >= value

characterMatches ::
  (HasGame m) => CharacterMatcher -> CharacterId -> m Bool
characterMatches matcher ident = member ident <$> inAnyArea (gameSelectCharacter matcher)

encounterCardMatches :: (HasGame m) => BasicEncounterCardMatcher -> EncounterCard -> m Bool
encounterCardMatches matcher card = case matcher of
  AnyEncounterCard -> pure True
  EncounterCardMatching m -> pure $ cardMatch m card
  -- with what The Next Evolution adds: having icons is no count of them
  EncounterCardWithBoostIcons -> (> 0) <$> getBoostIcons card
  EncounterCardWithBoostAbility -> pure $ Star `elem` cdBoostIcons (getCardDef card)

damageMatches :: (HasGame m) => DamageMatcher -> Damage -> m Bool
damageMatches matcher damage = case matcher of
  AttackFromPlayer identityMatcher -> case damageSource damage of
    FromPlayerAttack ident -> identityMatches identityMatcher ident
    _ -> pure False
  AttackFromAlly allyMatcher -> case damageSource damage of
    FromAllyAttack ident -> allyMatches allyMatcher ident
    _ -> pure False
  AnyDamage -> pure True
  AnyAttack -> pure case damageSource damage of
    FromPlayerAttack _ -> True
    FromAllyAttack _ -> True
    _ -> False
  -- who dealt it is known only from its source: see 'damageMatchesFrom'
  DamageByPlayer identityMatcher -> case damageSource damage of
    FromPlayerAttack ident -> identityMatches identityMatcher ident
    _ -> pure False

-- | Whether damage, dealt by the source given, matches.
damageMatchesFrom :: (HasGame m) => DamageMatcher -> Damage -> Source -> m Bool
damageMatchesFrom matcher damage source = case matcher of
  -- "you" is the identity (RR, "You"): what an ally does is not the player's doing
  DamageByPlayer _ | AllyRef _ <- source -> pure False
  DamageByPlayer identityMatcher ->
    getSourceController source >>= \case
      Just ident -> identityMatches identityMatcher ident
      Nothing -> damageMatches matcher damage
  _ -> damageMatches matcher damage

-- | Whether the source matches: an event by who played it, while it resolves.
sourceMatches :: (HasGame m) => SourceMatcher -> Source -> m Bool
sourceMatches matcher source = case matcher of
  SourceIsIdentity m | IdentityRef ident <- source -> identityMatches m ident
  EventPlayedBy m | EventRef _ <- source -> maybe (pure False) (identityMatches m) =<< getSourceController source
  SourceOneOf ms -> anyM (`sourceMatches` source) ms
  _ -> pure False

instance (HasGame m) => Count m IdentityMatcher where
  data QueryCount IdentityMatcher = SustainedDamage | HeroAttackDamage | HeroThwartValue
  selectCount = gameSelectCountIdentity

instance (HasGame m) => Count m SchemeMatcher where
  data QueryCount SchemeMatcher = SchemeThreat
  selectCount = gameSelectCountScheme

instance (HasGame m) => Count m AllyMatcher where
  data QueryCount AllyMatcher = AllyAttackValue | AllyThwartValue
  selectCount = gameSelectCountAlly
