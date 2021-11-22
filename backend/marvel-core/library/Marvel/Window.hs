module Marvel.Window where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Source

data WindowTiming = After | When | Would
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data DamageSource = FromAttack
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data RevealSource = FromEncounterDeck | FromVillain
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data WindowMatcher
  = PlayThis WindowTiming
  | WouldTakeDamage IdentityMatcher DamageSource GameValueMatcher
  | EnemyWouldAttack EnemyMatcher IdentityMatcher
  | EnemyAttacked EnemyMatcher IdentityMatcher
  | TreacheryRevealed WindowTiming TreacheryMatcher RevealSource
  | VillainRevealed WindowTiming VillainMatcher RevealSource
  | VillainDamaged WindowTiming VillainMatcher
  | MinionDefeated WindowTiming MinionMatcher
  | MinionEntersPlay WindowTiming MinionMatcher
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
  | IdentityTakeDamage IdentityId DamageSource Natural
  | RevealTreachery TreacheryId RevealSource
  | RevealVillain VillainId RevealSource
  | DamagedVillain VillainId Natural
  | DefeatedMinion MinionId
  | MinionEnteredPlay MinionId
  | EnemyAttack EnemyId IdentityId
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
  WouldTakeDamage identityMatcher damageSource gameValueMatcher ->
    case windowType w of
      IdentityTakeDamage ident damageSource' n
        | damageSource == damageSource' -> liftA2
          (&&)
          (identityMatches identityMatcher ident)
          (gameValueMatches gameValueMatcher n)
      _ -> pure False
  EnemyWouldAttack enemyMatcher identityMatcher -> case windowType w of
    EnemyAttack enemyId ident | windowTiming w == Would -> liftA2
      (&&)
      (identityMatches identityMatcher ident)
      (enemyMatches enemyMatcher enemyId)
    _ -> pure False
  EnemyAttacked enemyMatcher identityMatcher -> case windowType w of
    EnemyAttack enemyId ident | windowTiming w == When -> liftA2
      (&&)
      (identityMatches identityMatcher ident)
      (enemyMatches enemyMatcher enemyId)
    _ -> pure False
  TreacheryRevealed timing treacheryMatcher revealSource ->
    case windowType w of
      RevealTreachery treacheryId revealSource'
        | revealSource == revealSource' -> liftA2
          (&&)
          (treacheryMatches treacheryMatcher treacheryId)
          (pure $ timing == windowTiming w)
      _ -> pure False
  VillainRevealed timing villainMatcher revealSource -> case windowType w of
    RevealVillain villainId revealSource' | revealSource == revealSource' ->
      liftA2
        (&&)
        (villainMatches villainMatcher villainId)
        (pure $ timing == windowTiming w)
    _ -> pure False
  VillainDamaged timing villainMatcher -> case windowType w of
    DamagedVillain villainId _ | timing == windowTiming w ->
      villainMatches villainMatcher villainId
    _ -> pure False
  MinionDefeated timing minionMatcher -> case windowType w of
    DefeatedMinion minionId | windowTiming w == timing ->
      minionMatches minionMatcher minionId
    _ -> pure False
  MinionEntersPlay timing minionMatcher -> case windowType w of
    MinionEnteredPlay minionId | windowTiming w == timing ->
      minionMatches minionMatcher minionId
    _ -> pure False
  RoundEnds -> case windowType w of
    RoundEnded -> pure True
    _ -> pure False
