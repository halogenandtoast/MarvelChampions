module Marvel.Window (
  module Marvel.Window,
  module Marvel.Window.Types,
) where

import Marvel.Prelude

import Marvel.Attack (sourceEnemy)
import Marvel.Card.Def (cardMatch)
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
  EntersPlayThis -> case windowType w of
    AllyEnteredPlay allyId -> pure $ source == AllyRef allyId
    _ -> pure False
  HeroThwartsAllThreat identityMatcher -> case windowType w of
    ThwartedAllThreat ident _ -> identityMatches identityMatcher ident
    _ -> pure False
  AllyWouldTakeDamage allyMatcher -> case windowType w of
    AllyTakeDamage aid damage | damageAmount damage > 0 -> allyMatches allyMatcher aid
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
    EnemyAttacksAndDamages enemyId ident _
      | windowTiming w == After ->
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
  VillainDamagedBy timing villainMatcher damageMatcher -> case windowType w of
    DamagedVillain villainId damage
      | timing == windowTiming w ->
          liftA2 (&&) (villainMatches villainMatcher villainId) (damageMatches damageMatcher damage)
    _ -> pure False
  VillainSchemed timing villainMatcher -> case windowType w of
    Schemed villainId
      | timing == windowTiming w ->
          villainMatches villainMatcher villainId
    _ -> pure False
  MinionDefeated timing minionMatcher -> case windowType w of
    DefeatedMinion minionId _ _ _
      | windowTiming w == timing ->
          defeatedMinionMatches timing minionMatcher minionId
    _ -> pure False
  EnemyDefeated timing enemyMatcher damageMatcher -> case windowType w of
    DefeatedMinion minionId damage source _
      | windowTiming w == timing ->
          liftA2
            (&&)
            (if enemyMatcher `elem` [AnyEnemy, MinionEnemy] then pure True else enemyMatches (DefeatedEnemy enemyMatcher) (EnemyMinionId minionId))
            (damageMatchesFrom damageMatcher damage source)
    DefeatedVillain villainId damage source _
      | windowTiming w == timing ->
          liftA2
            (&&)
            (enemyMatches enemyMatcher (EnemyVillainId villainId))
            (damageMatchesFrom damageMatcher damage source)
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
  TurnEnds timing identityMatcher -> case windowType w of
    TurnEnded identityId
      | windowTiming w == timing ->
          identityMatches identityMatcher identityId
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
    -- the window after a side scheme's defeat says by whom: it has left play by then
    DefeatedSideSchemeBy sideSchemeId _ _
      | windowTiming w == timing ->
          defeatedSideSchemeMatches sideSchemeMatcher sideSchemeId
    _ -> pure False
  DefeatsSideScheme identityMatcher -> case windowType w of
    DefeatedSideSchemeBy _ identityId _
      | windowTiming w == After ->
          identityMatches identityMatcher identityId
    _ -> pure False
  MinionDefeatedBy timing minionMatcher sourceMatcher -> case windowType w of
    DefeatedMinion minionId _ source _
      | windowTiming w == timing ->
          liftA2 (&&) (defeatedMinionMatches timing minionMatcher minionId) (sourceMatches sourceMatcher source)
    _ -> pure False
  SideSchemeDefeatedBy sideSchemeMatcher sourceMatcher -> case windowType w of
    DefeatedSideSchemeBy sideSchemeId _ (Just source)
      | windowTiming w == After ->
          liftA2 (&&) (defeatedSideSchemeMatches sideSchemeMatcher sideSchemeId) (sourceMatches sourceMatcher source)
    _ -> pure False
  UsesRecovery identityMatcher -> case windowType w of
    UsedRecovery identityId
      | windowTiming w == After ->
          identityMatches identityMatcher identityId
    _ -> pure False
  IdentityReadies identityMatcher -> case windowType w of
    IdentityReadied identityId
      | windowTiming w == After ->
          identityMatches identityMatcher identityId
    _ -> pure False
  HeroDefended timing identityMatcher enemyMatcher -> case windowType w of
    HeroDefends identityId enemyId
      | windowTiming w == timing ->
          liftA2
            (&&)
            (identityMatches identityMatcher identityId)
            (enemyMatches enemyMatcher enemyId)
    _ -> pure False
  IdentityWouldBeDefeated identityMatcher -> case windowType w of
    IdentityDefeat identityId
      | windowTiming w == Would ->
          identityMatches identityMatcher identityId
    _ -> pure False
  TurnBegins identityMatcher -> case windowType w of
    TurnBegan identityId
      | windowTiming w == After ->
          identityMatches identityMatcher identityId
    _ -> pure False
  EncounterCardRevealedFromDeck timing encounterCardMatcher -> case windowType w of
    EncounterCardRevealed RevealedFromEncounterDeck _ encounterCard
      | windowTiming w == timing ->
          encounterCardMatches encounterCardMatcher encounterCard
    _ -> pure False
  HeroDefendedWithoutDamage timing identityMatcher -> case windowType w of
    DefendedWithoutDamage identityId _
      | windowTiming w == timing -> identityMatches identityMatcher identityId
    _ -> pure False
  PlaysEvent timing identityMatcher cardMatcher -> case windowType w of
    EventPlayed ident _ def
      | windowTiming w == timing && cardMatch cardMatcher def ->
          identityMatches identityMatcher ident
    _ -> pure False
  AllyDefeated timing allyMatcher -> case windowType w of
    DefeatedAlly allyId
      | windowTiming w == timing -> allyMatches allyMatcher allyId
    _ -> pure False
  BoostCardFlipped timing enemyMatcher -> case windowType w of
    FlippedBoostCard _ enemyId
      | windowTiming w == timing -> enemyMatches enemyMatcher enemyId
    _ -> pure False
  EncounterCardRevealedBy timing identityMatcher encounterCardMatcher -> case windowType w of
    EncounterCardRevealed _ ident encounterCard
      | windowTiming w == timing ->
          (&&) <$> encounterCardMatches encounterCardMatcher encounterCard <*> identityMatches identityMatcher ident
    _ -> pure False
  PlaysCard timing identityMatcher cardMatcher -> case windowType w of
    CardPlayed ident card
      | windowTiming w == timing && cardMatch cardMatcher card ->
          identityMatches identityMatcher ident
    _ -> pure False
  PlaysAlly allyMatcher -> case windowType w of
    PlayedAlly aid | windowTiming w == After -> allyMatches allyMatcher aid
    _ -> pure False
  PlaysCardFromHand identityMatcher cardMatcher -> case windowType w of
    PlayingCardFromHand ident card | cardMatch cardMatcher card -> identityMatches identityMatcher ident
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
    FlippedBoostCard card _ | windowTiming w == timing -> encounterCardMatches encounterCardMatcher card
    _ -> pure False
  BoostIconsCounted -> case windowType w of
    CountingBoostIcons _ -> pure True
    _ -> pure False
  TookDamage identityMatcher damageSource' enemyMatcher -> case windowType w of
    IdentityTookDamage ident from damage
      | windowTiming w == After && damageSource damage == damageSource'
      , Just enemy <- sourceEnemy from ->
          liftA2 (&&) (identityMatches identityMatcher ident) (enemyMatches enemyMatcher enemy)
    _ -> pure False
  AllyWasDefeated timing identityMatcher -> case windowType w of
    AllyCardDefeated ident _ | windowTiming w == timing -> identityMatches identityMatcher ident
    _ -> pure False
  SurgeWouldResolve -> case windowType w of
    SurgeResolves _ -> pure $ windowTiming w == Would
    _ -> pure False
  EngagesMinion timing identityMatcher minionMatcher -> case windowType w of
    EngagedMinion ident minionId
      | windowTiming w == timing ->
          liftA2
            (&&)
            (identityMatches identityMatcher ident)
            (minionMatches minionMatcher minionId)
    _ -> pure False
  MakesBasicThwart timing identityMatcher -> case windowType w of
    MadeBasicThwart identityId
      | windowTiming w == timing ->
          identityMatches identityMatcher identityId
    _ -> pure False
  WindowOneOf matchers -> anyM (\m -> windowMatches m w source) matchers

-- | After its defeat a minion has left play: it is matched as it was, among those just
-- removed. Any minion is one without looking, so the match holds once the removed are
-- cleared (an ability used in the window) or when it was removed from the game.
-- ponytail: a narrower matcher still needs it among the removed; carry the card in the
-- window if a card ever asks for one after such a clear.
defeatedMinionMatches :: (HasGame m) => WindowTiming -> MinionMatcher -> MinionId -> m Bool
defeatedMinionMatches timing matcher minionId = case matcher of
  AnyMinion -> pure True
  _ | timing == After -> minionMatches (LeftPlayMinion matcher) minionId
  _ -> minionMatches matcher minionId

-- | After its defeat a side scheme has left play: matched as 'defeatedMinionMatches' is.
defeatedSideSchemeMatches :: (HasGame m) => SideSchemeMatcher -> SideSchemeId -> m Bool
defeatedSideSchemeMatches matcher sideSchemeId = case matcher of
  AnySideScheme -> pure True
  _ -> sideSchemeMatches (LeftPlaySideScheme matcher) sideSchemeId
