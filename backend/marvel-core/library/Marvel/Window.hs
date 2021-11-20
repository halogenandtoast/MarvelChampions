module Marvel.Window where

import Marvel.Prelude

import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Source

data WindowTiming = After | When
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DamageSource = FromAttack
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RevealSource = FromEncounterDeck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data WindowMatcher
  = PlayThis WindowTiming
  | WouldTakeDamage IdentityMatcher DamageSource GameValueMatcher
  | EnemyWouldAttack EnemyMatcher IdentityMatcher
  | TreacheryRevealed WindowTiming TreacheryMatcher RevealSource
  | MinionDefeated WindowTiming MinionMatcher
  | MinionEntersPlay WindowTiming MinionMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  | DefeatedMinion MinionId
  | MinionEnteredPlay MinionId
  | EnemyAttack EnemyId IdentityId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  MinionDefeated timing minionMatcher -> case windowType w of
    DefeatedMinion minionId | windowTiming w == timing ->
      minionMatches minionMatcher minionId
    _ -> pure False
  MinionEntersPlay timing minionMatcher -> case windowType w of
    MinionEnteredPlay minionId | windowTiming w == timing ->
      minionMatches minionMatcher minionId
    _ -> pure False
