module Marvel.Window where

import Marvel.Prelude

import {-# SOURCE #-} Marvel.Card.EncounterCard
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Source

data WindowTiming = After | When | Would
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data RevealSource = FromEncounterDeck | FromVillain
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ThreatSource
  = ThreatFromVillain
  | ThreatFromMinion
  | ThreatFromAbility
  | ThreatFromAcceleration
  | AnyThreatSource
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowMatcher
  = PlayThis WindowTiming
  | WouldTakeDamage IdentityMatcher DamageSource GameValueMatcher
  | EnemyWouldAttack EnemyMatcher IdentityMatcher
  | ThreatWouldBePlaced ThreatSource SchemeMatcher
  | EnemyAttacked EnemyMatcher IdentityMatcher
  | IdentityAttacked WindowTiming IdentityMatcher EnemyMatcher
  | AllyThwarted WindowTiming AllyMatcher SchemeMatcher
  | TreacheryRevealed WindowTiming TreacheryMatcher RevealSource
  | VillainRevealed WindowTiming VillainMatcher RevealSource
  | VillainDamaged WindowTiming VillainMatcher
  | MinionDefeated WindowTiming MinionMatcher
  | MinionEntersPlay WindowTiming MinionMatcher
  | EnemyDefeated WindowTiming EnemyMatcher DamageMatcher
  | IdentityChangedToForm WindowTiming IdentityMatcher
  | MakesBasicAttack WindowTiming IdentityMatcher
  | SideSchemeDefeated WindowTiming SideSchemeMatcher
  | HeroDefended WindowTiming IdentityMatcher EnemyMatcher
  | EncounterCardReveal WindowTiming BasicEncounterCardMatcher
  | RoundEnds
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data Window = Window
  { windowTiming :: WindowTiming
  , windowType :: WindowType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data WindowType
  = PlayedAlly AllyId
  | PlayedSupport SupportId
  | IdentityTakeDamage IdentityId Damage
  | RevealTreachery TreacheryId RevealSource
  | RevealVillain VillainId RevealSource
  | DamagedVillain VillainId Damage
  | DefeatedVillain VillainId Damage
  | DefeatedMinion MinionId Damage
  | MinionEnteredPlay MinionId
  | EnemyAttack EnemyId IdentityId
  | IdentityAttack IdentityId EnemyId
  | AllyThwart AllyId SchemeId
  | ThreatPlaced ThreatSource SchemeId Natural
  | IdentityChangesForm IdentityId
  | MadeBasicAttack IdentityId
  | DefeatedSideScheme SideSchemeId
  | HeroDefends IdentityId EnemyId
  | EncounterCardRevealed IdentityId EncounterCard
  | RoundEnded
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

windowMatches :: MonadGame env m => WindowMatcher -> Window -> Source -> m Bool
windowMatches matcher w source = case matcher of
  PlayThis timing -> case windowType w of
    PlayedAlly allyId ->
      pure $ timing == windowTiming w && source == AllySource allyId
    PlayedSupport supportId ->
      pure $ timing == windowTiming w && source == SupportSource supportId
    _ -> pure False
  WouldTakeDamage identityMatcher damageSource' gameValueMatcher ->
    case windowType w of
      IdentityTakeDamage ident damage
        | damageSource damage == damageSource' ->
          liftA2
            (&&)
            (identityMatches identityMatcher ident)
            (gameValueMatches gameValueMatcher (damageAmount damage))
      _ -> pure False
  EnemyWouldAttack enemyMatcher identityMatcher -> case windowType w of
    EnemyAttack enemyId ident
      | windowTiming w == Would ->
        liftA2
          (&&)
          (identityMatches identityMatcher ident)
          (enemyMatches enemyMatcher enemyId)
    _ -> pure False
  EnemyAttacked enemyMatcher identityMatcher -> case windowType w of
    EnemyAttack enemyId ident
      | windowTiming w == When ->
        liftA2
          (&&)
          (identityMatches identityMatcher ident)
          (enemyMatches enemyMatcher enemyId)
    _ -> pure False
  IdentityAttacked timing identityMatcher enemyMatcher -> case windowType w of
    IdentityAttack ident enemyId
      | windowTiming w == timing ->
        liftA2
          (&&)
          (identityMatches identityMatcher ident)
          (enemyMatches enemyMatcher enemyId)
    _ -> pure False
  AllyThwarted timing allyMatcher schemeMatcher -> case windowType w of
    AllyThwart allyId schemeId
      | windowTiming w == timing ->
        liftA2
          (&&)
          (allyMatches allyMatcher allyId)
          (schemeMatches schemeMatcher schemeId)
    _ -> pure False
  TreacheryRevealed timing treacheryMatcher revealSource ->
    case windowType w of
      RevealTreachery treacheryId revealSource'
        | revealSource == revealSource' ->
          liftA2
            (&&)
            (treacheryMatches treacheryMatcher treacheryId)
            (pure $ timing == windowTiming w)
      _ -> pure False
  VillainRevealed timing villainMatcher revealSource -> case windowType w of
    RevealVillain villainId revealSource'
      | revealSource == revealSource' ->
        liftA2
          (&&)
          (villainMatches villainMatcher villainId)
          (pure $ timing == windowTiming w)
    _ -> pure False
  VillainDamaged timing villainMatcher -> case windowType w of
    DamagedVillain villainId _
      | timing == windowTiming w ->
        villainMatches villainMatcher villainId
    _ -> pure False
  MinionDefeated timing minionMatcher -> case windowType w of
    DefeatedMinion minionId _
      | windowTiming w == timing ->
        if timing == After
          then case minionMatcher of
            AnyMinion -> pure True
            _ -> error "Minion has been deleted so we do not have details"
          else minionMatches minionMatcher minionId
    _ -> pure False
  EnemyDefeated timing enemyMatcher damageMatcher -> case windowType w of
    DefeatedMinion minionId damage
      | windowTiming w == timing ->
        if timing == After
          then case enemyMatcher of
            MinionEnemy -> damageMatches damageMatcher damage
            AnyEnemy -> damageMatches damageMatcher damage
            _ -> error "Minion has been deleted so we do not have details"
          else liftA2 (&&) (enemyMatches enemyMatcher (EnemyMinionId minionId)) (damageMatches damageMatcher damage)
    DefeatedVillain villainId damage
      | windowTiming w == timing ->
        if timing == After
          then case enemyMatcher of
            VillainEnemy -> damageMatches damageMatcher damage
            AnyEnemy -> damageMatches damageMatcher damage
            _ -> error "Minion has been deleted so we do not have details"
          else liftA2 (&&) (enemyMatches enemyMatcher (EnemyVillainId villainId)) (damageMatches damageMatcher damage)
    _ -> pure False
  MinionEntersPlay timing minionMatcher -> case windowType w of
    MinionEnteredPlay minionId
      | windowTiming w == timing ->
        minionMatches minionMatcher minionId
    _ -> pure False
  ThreatWouldBePlaced threatSource schemeMatcher -> case windowType w of
    ThreatPlaced threatSource' schemeId _
      | (windowTiming w == Would)
          && (threatSource == AnyThreatSource || threatSource == threatSource') ->
        schemeMatches schemeMatcher schemeId
    _ -> pure False
  RoundEnds -> case windowType w of
    RoundEnded -> pure True
    _ -> pure False
  IdentityChangedToForm timing identityMatcher -> case windowType w of
    IdentityChangesForm identityId
      | windowTiming w == timing ->
        identityMatches identityMatcher identityId
    _ -> pure False
  MakesBasicAttack timing identityMatcher -> case windowType w of
    MadeBasicAttack identityId
      | windowTiming w == timing ->
        identityMatches identityMatcher identityId
    _ -> pure False
  SideSchemeDefeated timing sideSchemeMatcher -> case windowType w of
    DefeatedSideScheme sideSchemeId
      | windowTiming w == timing ->
        sideSchemeMatches sideSchemeMatcher sideSchemeId
    _ -> pure False
  HeroDefended timing identityMatcher enemyMatcher -> case windowType w of
    HeroDefends identityId enemyId
      | windowTiming w == timing ->
        liftA2
          (&&)
          (identityMatches identityMatcher identityId)
          (enemyMatches enemyMatcher enemyId)
    _ -> pure False
  EncounterCardReveal timing encounterCardMatcher -> case windowType w of
    EncounterCardRevealed _ encounterCard
      | windowTiming w == timing ->
        pure $ encounterCardMatches encounterCardMatcher encounterCard
    _ -> pure False
