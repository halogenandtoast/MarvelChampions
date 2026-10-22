module Marvel.Window (
  module Marvel.Window,
  module Marvel.Window.Types,
) where

import Marvel.Prelude

import Marvel.Card.Def
import Marvel.Card.PlayerCard.Types
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Ref
import Marvel.Window.Types

windowMatches :: (HasGame m) => WindowMatcher -> Window -> Source -> m Bool
windowMatches matcher w source = case matcher of
  PlayThis timing -> case windowType w of
    PlayedAlly allyId ->
      pure $ timing == windowTiming w && source == AllyRef allyId
    PlayedSupport supportId ->
      pure $ timing == windowTiming w && source == SupportRef supportId
    _ -> pure False
  WouldTakeDamage identityMatcher damageSource' gameValueMatcher ->
    case windowType w of
      IdentityTakeDamage ident damage
        | damageSource' `elem` [damageSource damage, FromAnyDamageSource] ->
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
  EnemyAttacked timing enemyMatcher identityMatcher -> case windowType w of
    EnemyAttack enemyId ident
      | windowTiming w == timing ->
          liftA2
            (&&)
            (identityMatches identityMatcher ident)
            (enemyMatches enemyMatcher enemyId)
    _ -> pure False
  EnemyAttackedAndDamaged enemyMatcher characterMatcher -> case windowType w of
    EnemyAttacksAndDamages enemyId ident
      | windowTiming w == When ->
          liftA2
            (&&)
            (characterMatches characterMatcher ident)
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
  AllyAttacked timing allyMatcher enemyMatcher -> case windowType w of
    AllyAttack allyId enemyId
      | windowTiming w == timing ->
          liftA2
            (&&)
            (allyMatches allyMatcher allyId)
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
  VillainSchemed timing villainMatcher -> case windowType w of
    Schemed villainId
      | timing == windowTiming w ->
          villainMatches villainMatcher villainId
    _ -> pure False
  MinionDefeated timing minionMatcher -> case windowType w of
    DefeatedMinion minionId _ _
      | windowTiming w == timing ->
          minionMatches minionMatcher minionId
    _ -> pure False
  EnemyDefeated timing enemyMatcher damageMatcher -> case windowType w of
    DefeatedMinion minionId damage source
      | windowTiming w == timing ->
          liftA2
            (&&)
            (enemyMatches (DefeatedEnemy enemyMatcher) (EnemyMinionId minionId))
            (damageMatchesFrom damageMatcher damage source)
    DefeatedVillain villainId damage
      | windowTiming w == timing ->
          liftA2
            (&&)
            (enemyMatches enemyMatcher (EnemyVillainId villainId))
            (damageMatches damageMatcher damage)
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
  EncounterCardRevealedBy timing identityMatcher encounterCardMatcher -> case windowType w of
    EncounterCardRevealed ident encounterCard
      | windowTiming w == timing && encounterCardMatches encounterCardMatcher encounterCard ->
          identityMatches identityMatcher ident
    _ -> pure False
  PlaysCard timing identityMatcher cardMatcher -> case windowType w of
    CardPlayed ident card
      | windowTiming w == timing && cardMatch cardMatcher card ->
          identityMatches identityMatcher ident
    _ -> pure False
  ResolvesAbility timing identityMatcher cardMatcher -> case windowType w of
    ResolvedAbility ident def
      | windowTiming w == timing && cardMatch cardMatcher def ->
          identityMatches identityMatcher ident
    _ -> pure False
  MinionHasSchemed timing minionMatcher -> case windowType w of
    SchemedByMinion minionId
      | windowTiming w == timing -> minionMatches minionMatcher minionId
    _ -> pure False
  BoostCardTurnedFaceup timing encounterCardMatcher -> case windowType w of
    BoostCardFlipped _ card ->
      pure $ windowTiming w == timing && encounterCardMatches encounterCardMatcher card
    _ -> pure False
  TookDamage identityMatcher damageSource' -> case windowType w of
    IdentityTookDamage ident _ damage
      | windowTiming w == After && damageSource damage == damageSource' ->
          identityMatches identityMatcher ident
    _ -> pure False
  AllyWasDefeated timing identityMatcher -> case windowType w of
    DefeatedAlly ident _ | windowTiming w == timing -> identityMatches identityMatcher ident
    _ -> pure False
  SurgeWouldResolve -> case windowType w of
    SurgeResolves _ -> pure $ windowTiming w == Would
    _ -> pure False
