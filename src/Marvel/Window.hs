module Marvel.Window (
  module Marvel.Window,
  module Marvel.Window.Types,
) where

import Marvel.Prelude

import Marvel.Card.Def (sameCard)
import Marvel.Card.PlayerCard.Types (pcCardId)
import Marvel.Card.Id (CardId (..), HasCardId (toCardId))

import Marvel.Attack (sourceEnemy)
import Marvel.Card.Def (cardMatch)
import Marvel.Status (StatusCard (ToughStatus))
import Marvel.Modifier.Types (Modifier (DirectedForceApplied))
import Marvel.Damage
import Marvel.Keyword (Keyword (Ranged))
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Ref
import Marvel.Window.Types

windowMatches :: (HasGame m) => WindowMatcher -> Window -> Source -> m Bool
windowMatches matcher w source = case matcher of
  EnemyDefeatedByBasicAttack identityMatcher -> case windowType w of
    DefeatedMinion _ damage (IdentityRef you) _
      | windowTiming w == After, damageSource damage == FromPlayerAttack you -> identityMatches identityMatcher you
    DefeatedVillain _ damage (IdentityRef you) _
      | windowTiming w == After, damageSource damage == FromPlayerAttack you -> identityMatches identityMatcher you
    _ -> pure False
  CharacterTookIndirectDamage characterMatcher -> case windowType w of
    IdentityTookDamage ident _ damage | windowTiming w == After, damageIndirect damage, damageAmount damage > 0 -> characterMatches characterMatcher (IdentityCharacter ident)
    AllyTookDamage aid _ damage | windowTiming w == After, damageIndirect damage, damageAmount damage > 0 -> characterMatches characterMatcher (AllyCharacter aid)
    _ -> pure False
  EnemyWouldBeDealtDamage matcher' -> case windowType w of
    EnemyDamageWouldBeDealt enemy _ damage | windowTiming w == When, damageAmount damage > 0 -> enemyMatches matcher' enemy
    _ -> pure False
  EnemyWouldBeDealtDamageBy matcher' damageMatcher -> case windowType w of
    EnemyDamageWouldBeDealt enemy from damage | windowTiming w == When, damageAmount damage > 0 ->
      (&&) <$> enemyMatches matcher' enemy <*> damageMatchesFrom damageMatcher damage from
    _ -> pure False
  PlayerCardEntersPlay timing identityMatcher cardMatcher -> case windowType w of
    PlayerCardEnteredPlay you card | timing == windowTiming w -> (&&) <$> identityMatches identityMatcher you <*> pure (cardMatch cardMatcher card)
    _ -> pure False
  HeroDefendedWithoutDamageAgainst timing identMatcher enemyMatcher -> case windowType w of
    DefendedWithoutDamage ident enemy | windowTiming w == timing -> (&&) <$> identityMatches identMatcher ident <*> enemyMatches enemyMatcher enemy
    _ -> pure False
  EnemyWouldTakeDamage matcher' -> case windowType w of
    EnemyWouldBeDamaged enemy _ damage | windowTiming w == When, damageAmount damage > 0 -> do
      let character = case enemy of EnemyVillainId v -> VillainCharacter v; EnemyMinionId m -> MinionCharacter m
      tough <- characterMatches (CharacterWithStatus ToughStatus) character
      if tough && preventedByTough damage then pure False else enemyMatches matcher' enemy
    _ -> pure False
  MainSchemeWouldComplete matcher' -> case windowType w of
    MainSchemeCompleting mid | windowTiming w == Would -> schemeMatches matcher' (SchemeMainSchemeId mid)
    _ -> pure False
  BasicAttackDamagedEnemy matcher' -> case windowType w of
    BasicAttackDamaged you enemies | windowTiming w == After && notNull enemies -> identityMatches matcher' you
    _ -> pure False
  AllyAttackDamagedEnemy allyMatcher enemyMatcher -> case windowType w of
    AllyAttackDamaged aid enemy confused | windowTiming w == After -> do
      matchesAlly <- allyMatches allyMatcher aid
      matchesEnemy <- if enemyMatcher == ConfusedEnemy then pure confused else enemyMatches (DefeatedEnemy enemyMatcher) enemy
      pure $ matchesAlly && matchesEnemy
    _ -> pure False
  MakesKeywordAttack timing matcher' keywords -> case windowType w of
    MakingFriendlyKeywordAttack you from _ present | windowTiming w == timing && any (`elem` present) keywords -> do
      used <- elem DirectedForceApplied <$> getModifiers from
      if used then pure False else identityMatches matcher' you
    MakingKeywordAttack you from _ present _ | windowTiming w == timing && any (`elem` present) keywords -> do
      used <- elem DirectedForceApplied <$> getModifiers from
      if used then pure False else identityMatches matcher' you
    _ -> pure False
  MakesRangedAttack timing matcher' -> case windowType w of
    MakingRangedAttack you _ _ | windowTiming w == timing -> identityMatches matcher' you
    MakingFriendlyKeywordAttack you _ _ keywords | windowTiming w == timing, Ranged `elem` keywords -> identityMatches matcher' you
    _ -> pure False
  CardsDiscardedBy identityMatcher -> case windowType w of
    PlayerDiscardedFromEncounterDeck actor card | windowTiming w == After -> (&&) <$> identityMatches identityMatcher actor <*> discardResponseAvailable (toCardId card)
    PlayerDiscardedFromDeck actor _ card | windowTiming w == After -> (&&) <$> identityMatches identityMatcher actor <*> discardResponseAvailable (pcCardId card)
    _ -> pure False
  CardsDiscardedFromPlayerDeck identityMatcher -> case windowType w of
    DiscardedFromPlayerDeck ident cards | windowTiming w == After && not (null cards) -> allM (discardResponseAvailable . pcCardId) cards >>= \available -> if available then identityMatches identityMatcher ident else pure False
    _ -> pure False
  ResourcesGeneratedBy identityMatcher -> case windowType w of
    GeneratedResources ident n | windowTiming w == After && n > 0 -> identityMatches identityMatcher ident
    _ -> pure False
  TreacheryResolvedBy identityMatcher -> case windowType w of
    ResolvedTreacheryCard ident _ | windowTiming w == After -> identityMatches identityMatcher ident
    _ -> pure False
  IdentityThwarts timing identityMatcher -> case windowType w of
    IdentityThwart ident _ | windowTiming w == timing -> identityMatches identityMatcher ident
    _ -> pure False
  EnemyAttackedAnyTarget timing enemyMatcher -> case windowType w of
    EnemyAttack enemy _ | windowTiming w == timing -> enemyMatches enemyMatcher enemy
    EnemyAttackAlly enemy _ _ | windowTiming w == timing -> enemyMatches enemyMatcher enemy
    EnemyAttackOnEnemy enemy _ | windowTiming w == timing -> enemyMatches enemyMatcher enemy
    _ -> pure False
  CharacterDefeatedByEnemyAttack timing enemyMatcher -> case windowType w of
    EnemyAttackDefeatedCharacter enemy _ | windowTiming w == timing -> enemyMatches enemyMatcher enemy
    DefeatedAlly _ (Just enemy) | windowTiming w == timing -> enemyMatches enemyMatcher enemy
    DefeatedMinion _ damage from _
      | windowTiming w == timing, damageSource damage == FromAttack, Just enemy <- sourceEnemy from -> enemyMatches enemyMatcher enemy
    DefeatedVillain _ damage from _
      | windowTiming w == timing, damageSource damage == FromAttack, Just enemy <- sourceEnemy from -> enemyMatches enemyMatcher enemy
    _ -> pure False
  MinionWouldLeavePlay minionMatcher -> case windowType w of
    MinionLeavingPlay mid | windowTiming w == Would -> minionMatches minionMatcher mid
    _ -> pure False
  HeroThwarted timing identityMatcher -> case windowType w of
    HeroThwartComplete ident | windowTiming w == timing -> identityMatches identityMatcher ident
    MadeBasicThwart ident | windowTiming w == timing -> identityMatches identityMatcher ident
    _ -> pure False
  IdentityDamaged timing identityMatcher -> case windowType w of
    IdentityTookDamage ident _ damage | windowTiming w == timing && damageAmount damage > 0 -> identityMatches identityMatcher ident
    _ -> pure False
  IdentityDamagedByAttack timing identityMatcher -> case windowType w of
    IdentityTookDamage ident _ damage
      | windowTiming w == timing, damageAmount damage > 0
      , damageSource damage `elem` [FromAttack, FromOverkill] -> identityMatches identityMatcher ident
    _ -> pure False
  ThreatRemovedByThwart timing sourceMatcher schemeMatcher -> case windowType w of
    ThwartRemovedThreat from scheme _ removed | windowTiming w == timing && removed > 0 ->
      (&&) <$> sourceMatches sourceMatcher from <*> schemeMatches schemeMatcher scheme
    _ -> pure False
  RemovesLastThreat identityMatcher -> case windowType w of
    LastThreatRemoved ident _ | windowTiming w == After -> identityMatches identityMatcher ident
    _ -> pure False
  PhaseBegins timing phase -> pure $ windowTiming w == timing && windowType w == PhaseBegan phase
  PhaseEnds timing phase -> pure $ windowTiming w == timing && windowType w == PhaseEnded phase
  PlayThis timing -> case windowType w of
    PlayedAlly allyId ->
      pure $ timing == windowTiming w && source == AllyRef allyId
    PlayedSupport supportId ->
      pure $ timing == windowTiming w && source == SupportRef supportId
    CardPlayed _ card | UpgradeRef uid <- source ->
      pure $ timing == windowTiming w && pcCardId card == CardId (unUpgradeId uid)
    _ -> pure False
  AllyDefendsAgainst timing allyMatcher enemyMatcher -> case windowType w of
    AllyDefendedAgainst aid enemy | windowTiming w == timing ->
      (&&) <$> allyMatches allyMatcher aid <*> enemyMatches enemyMatcher enemy
    _ -> pure False
  IdentityWouldTakeAttackDamageFrom enemyMatcher identityMatcher -> case windowType w of
    IdentityTakeDamageFrom ident damageSourceRef damage
      | windowTiming w == When, damageAmount damage > 0
      , damageSource damage `elem` [FromAttack, FromOverkill]
      , Just enemy <- sourceEnemy damageSourceRef ->
          (&&) <$> identityMatches identityMatcher ident <*> enemyMatches enemyMatcher enemy
    _ -> pure False
  EntersPlayThis -> case windowType w of
    AllyEnteredPlay allyId -> pure $ source == AllyRef allyId
    _ -> pure False
  HeroThwartsAllThreat identityMatcher -> case windowType w of
    ThwartedAllThreat ident _ -> identityMatches identityMatcher ident
    _ -> pure False
  CharacterWithConditionWouldTakeDamage statuses -> case windowType w of
    IdentityTakeDamage you damage | windowTiming w == When, damageAmount damage > 0 ->
      anyM (\status -> characterHasCondition status $ IdentityCharacter you) statuses
    AllyTakeDamage aid damage | windowTiming w == When, damageAmount damage > 0 ->
      anyM (\status -> characterHasCondition status $ AllyCharacter aid) statuses
    _ -> pure False
  CharacterWouldTakeAttackDamageFrom enemyMatcher characterMatcher -> case windowType w of
    IdentityTakeDamageFrom ident source damage | windowTiming w == When, damageAmount damage > 0, damageSource damage == FromAttack, Just enemy <- sourceEnemy source ->
      (&&) <$> enemyMatches enemyMatcher enemy <*> characterMatches characterMatcher (IdentityCharacter ident)
    AllyTakeDamageFrom aid source damage | windowTiming w == When, damageAmount damage > 0, damageSource damage == FromAttack, Just enemy <- sourceEnemy source ->
      (&&) <$> enemyMatches enemyMatcher enemy <*> characterMatches characterMatcher (AllyCharacter aid)
    _ -> pure False
  CharacterWouldTakeAttackDamage characterMatcher ->
    let attackDamage damage = windowTiming w == When && damageAmount damage > 0 && case damageSource damage of
          FromAttack -> True
          FromOverkill -> True
          FromPlayerAttack _ -> True
          FromAllyAttack _ -> True
          _ -> False
     in case windowType w of
       IdentityTakeDamage ident damage | attackDamage damage -> characterMatches characterMatcher (IdentityCharacter ident)
       AllyTakeDamage aid damage | attackDamage damage -> characterMatches characterMatcher (AllyCharacter aid)
       EnemyWouldBeDamaged enemy _ damage | attackDamage damage -> characterMatches characterMatcher $ case enemy of
         EnemyVillainId vid -> VillainCharacter vid
         EnemyMinionId mid -> MinionCharacter mid
       _ -> pure False
  AllyWouldTakeDamageOf timing allyMatcher source -> case windowType w of
    AllyTakeDamage aid damage
      | windowTiming w == timing && damageAmount damage > 0 && source `elem` [damageSource damage, FromAnyDamageSource] -> allyMatches allyMatcher aid
    _ -> pure False
  AllyWouldTakeDamage allyMatcher -> case windowType w of
    AllyTakeDamage aid damage | damageAmount damage > 0 -> allyMatches allyMatcher aid
    _ -> pure False
  AllyWouldTakeDamageEqualToRemainingHitPoints allyMatcher -> case windowType w of
    AllyTakeDamage aid damage | damageAmount damage > 0 -> allyMatches (allyMatcher <> AllyWithRemainingHitPoints (damageAmount damage)) aid
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
  VillainWouldBeDefeatedMatching matcher' -> case windowType w of
    VillainDefeatWouldOccur vid | windowTiming w == Would -> villainMatches matcher' vid
    _ -> pure False
  EnemyAttacksPlayerOrAlly timing enemyMatcher identityMatcher -> case windowType w of
    EnemyAttack enemy ident | windowTiming w == timing ->
      (&&) <$> enemyMatches enemyMatcher enemy <*> identityMatches identityMatcher ident
    EnemyAttackAlly enemy _ ident | windowTiming w == timing ->
      (&&) <$> enemyMatches enemyMatcher enemy <*> identityMatches identityMatcher ident
    _ -> pure False
  EnemyWouldAttack enemyMatcher identityMatcher -> case windowType w of
    EnemyAttack enemyId ident
      | windowTiming w == Would -> do
          stillIdentity <- enemyAttackTargetsIdentity enemyId ident
          if not stillIdentity then pure False else
            (&&) <$> identityMatches identityMatcher ident <*> enemyMatches enemyMatcher enemyId
    _ -> pure False
  EnemyAttacked timing enemyMatcher identityMatcher -> case windowType w of
    EnemyAttackAlly enemyId _ ident | windowTiming w == timing ->
      (&&) <$> identityMatches identityMatcher ident <*> enemyMatches enemyMatcher enemyId
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
  IdentityTargetsEnemy timing identityMatcher enemyMatcher -> case windowType w of
    IdentityAttackTarget ident enemyId | windowTiming w == timing ->
      (&&) <$> identityMatches identityMatcher ident <*> enemyMatches enemyMatcher enemyId
    _ -> pure False
  IdentityAttacked timing identityMatcher enemyMatcher -> case windowType w of
    FriendlyPlayerAttack ident _ | windowTiming w == timing && enemyMatcher == AnyEnemy -> identityMatches identityMatcher ident
    FriendlyCharacterAttack (IdentityCharacter ident) _
      | windowTiming w == timing && enemyMatcher == AnyEnemy -> identityMatches identityMatcher ident
    IdentityAttack ident enemyId
      | windowTiming w == timing ->
          liftA2
            (&&)
            (identityMatches identityMatcher ident)
            (enemyMatches (if timing == After then DefeatedEnemy enemyMatcher else enemyMatcher) enemyId)
    _ -> pure False
  AllyAttacked timing allyMatcher enemyMatcher -> case windowType w of
    FriendlyCharacterAttack (AllyCharacter aid) _
      | windowTiming w == timing && enemyMatcher == AnyEnemy -> allyMatches allyMatcher aid
    AllyAttack allyId enemyId
      | windowTiming w == timing ->
          liftA2
            (&&)
            (allyMatches allyMatcher allyId)
            (enemyMatches (if timing == After then DefeatedEnemy enemyMatcher else enemyMatcher) enemyId)
    AllyAttackCompletedAgainst allyId enemies | windowTiming w == timing ->
      liftA2 (&&) (allyMatches allyMatcher allyId) (anyM (enemyMatches $ if timing == After then DefeatedEnemy enemyMatcher else enemyMatcher) enemies)
    _ -> pure False
  -- A single attack may resolve against several enemies. Its After window is one
  -- triggering condition; a target matcher succeeds if any resolved enemy matches.
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
  -- "when the villain would take damage": a tough status card's interrupt is first (RR
  -- Simultaneous Timing Priority), and with it preventing all the damage there is none
  -- to take, so the damage it prevents is not answered
  VillainDamaged timing villainMatcher -> case windowType w of
    DamagedVillain villainId damage
      | timing == windowTiming w -> do
          would <- if timing == When then wouldTakeDamage villainId damage else pure True
          (would &&) <$> villainMatches villainMatcher villainId
    _ -> pure False
  VillainWouldTakeDamage villainMatcher damageMatcher -> case windowType w of
    DamagedVillain villainId damage
      | windowTiming w == When -> do
          would <- wouldTakeDamage villainId damage
          if would then liftA2 (&&) (villainMatches villainMatcher villainId) (damageMatches damageMatcher damage) else pure False
    _ -> pure False
  VillainWouldTakeDamageFrom villainMatcher damageMatcher -> case windowType w of
    VillainDamageFrom villainId damageSourceRef damage | windowTiming w == When -> do
      would <- wouldTakeDamage villainId damage
      if would then (&&) <$> villainMatches villainMatcher villainId <*> damageMatchesFrom damageMatcher damage damageSourceRef else pure False
    _ -> pure False
  VillainDamagedBy timing villainMatcher damageMatcher -> case windowType w of
    DamagedVillain villainId damage
      | timing == windowTiming w ->
          liftA2 (&&) (villainMatches villainMatcher villainId) (damageMatches damageMatcher damage)
    _ -> pure False
  VillainWasSwapped timing villainMatcher -> case windowType w of
    VillainSwapped villainId | timing == windowTiming w -> villainMatches villainMatcher villainId
    _ -> pure False
  VillainIsDefeated timing villainMatcher -> case windowType w of
    VillainDefeat villainId | timing == windowTiming w -> villainMatches villainMatcher villainId
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
  EnemyTookDamage enemyMatcher damageMatcher -> case windowType w of
    EnemyTookDamageFrom enemy damage source
      | windowTiming w == After ->
          liftA2 (&&) (enemyMatches enemyMatcher enemy) (damageMatchesFrom damageMatcher damage source)
    _ -> pure False
  EnemyDefeatedWithUpgrade timing def damageMatcher -> case windowType w of
    DefeatedEnemyWithUpgrades _ defs damage source | windowTiming w == timing ->
      (any (sameCard def) defs &&) <$> damageMatchesFrom damageMatcher damage source
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
            (if enemyMatcher `elem` [AnyEnemy, VillainEnemy] then pure True else enemyMatches (DefeatedEnemy enemyMatcher) (EnemyVillainId villainId))
            (damageMatchesFrom damageMatcher damage source)
    _ -> pure False
  SideSchemeEntersPlay timing schemeMatcher -> case windowType w of
    SideSchemeEnteredPlay sid | windowTiming w == timing -> sideSchemeMatches schemeMatcher sid
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
  EnemyActivates timing enemyMatcher identityMatcher -> case windowType w of
    EnemyActivation enemyId identityId
      | windowTiming w == timing ->
          liftA2 (&&) (enemyMatches enemyMatcher enemyId) (identityMatches identityMatcher identityId)
    _ -> pure False
  PlayerDeckResets timing identityMatcher -> case windowType w of
    PlayerDeckReset identityId
      | windowTiming w == timing ->
          identityMatches identityMatcher identityId
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
  IdentityChangedAnyForm timing identityMatcher -> case windowType w of
    IdentityChangesForm identityId
      | windowTiming w == timing ->
          identityMatches identityMatcher identityId
    EnergyFormChanged identityId _
      | windowTiming w == timing ->
          identityMatches identityMatcher identityId
    MassFormChanged identityId _
      | windowTiming w == timing ->
          identityMatches identityMatcher identityId
    _ -> pure False
  WeatherSpecialResolvedFor timing identityMatcher -> case windowType w of
    WeatherSpecialResolved ident _ | windowTiming w == timing -> identityMatches identityMatcher ident
    _ -> pure False
  EnergyFormChangedTo timing upgradeMatcher -> case windowType w of
    EnergyFormChanged _ upgradeId
      | windowTiming w == timing ->
          upgradeMatches upgradeMatcher upgradeId
    _ -> pure False
  IdentityAttackResolved matcher -> case windowType w of
    PlayerAttackResolved source ident _ | windowTiming w == After -> case source of
      AllyRef _ -> pure False
      _ -> identityMatches matcher ident
    _ -> pure False
  VillainWouldActivate matcher -> case windowType w of
    VillainActivationWouldBegin _ ident | windowTiming w == Would -> identityMatches matcher ident
    _ -> pure False
  IdentityIgnoresKeyword timing im mm -> case windowType w of
    IgnoredKeyword ident mid | windowTiming w == timing -> liftA2 (&&) (identityMatches im ident) (minionMatches mm mid)
    _ -> pure False
  IdentityIgnoresCrisis timing im sm -> case windowType w of
    IgnoredCrisis ident sid | windowTiming w == timing -> liftA2 (&&) (identityMatches im ident) (sideSchemeMatches sm sid)
    _ -> pure False
  IdentityDiscardsTough timing identityMatcher -> case windowType w of
    IdentityDiscardedTough ident | windowTiming w == timing -> identityMatches identityMatcher ident
    _ -> pure False
  UpgradeLosesLastCounter timing upgradeMatcher -> case windowType w of
    UpgradeCounterAmountChanged uid old new | windowTiming w == timing, old > 0, new == 0 -> upgradeMatches upgradeMatcher uid
    _ -> pure False
  UpgradeCountersPlaced timing upgradeMatcher -> case windowType w of
    UpgradeCounterAmountChanged uid old new | windowTiming w == timing, new > old -> upgradeMatches upgradeMatcher uid
    _ -> pure False
  MassFormChangedTo timing upgradeMatcher -> case windowType w of
    MassFormChanged _ upgradeId
      | windowTiming w == timing -> upgradeMatches upgradeMatcher upgradeId
    _ -> pure False
  MakesBasicAttackAgainst timing identityMatcher enemyMatcher -> case windowType w of
    MadeBasicAttackAgainst identityId enemyId
      | windowTiming w == timing ->
          liftA2 (&&) (identityMatches identityMatcher identityId) (enemyMatches enemyMatcher enemyId)
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
  SideSchemeDefeatedByThwart schemeMatcher sourceMatcher -> case windowType w of
    DefeatedSideSchemeByThwart sid source | windowTiming w == After ->
      (&&) <$> defeatedSideSchemeMatches schemeMatcher sid <*> sourceMatches sourceMatcher source
    _ -> pure False
  SideSchemeDefeatedBy sideSchemeMatcher sourceMatcher -> case windowType w of
    DefeatedSideSchemeBy sideSchemeId _ (Just source)
      | windowTiming w == After ->
          liftA2 (&&) (defeatedSideSchemeMatches sideSchemeMatcher sideSchemeId) (sourceMatches sourceMatcher source)
    _ -> pure False
  UsesRecoveryAt timing matcher -> case windowType w of
    UsedRecovery ident | windowTiming w == timing -> identityMatches matcher ident
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
  HeroAttackDealsExcessDamage identityMatcher -> case windowType w of
    ExcessDamageDealt identityId _ n
      | windowTiming w == When && n > 0 ->
          identityMatches identityMatcher identityId
    _ -> pure False
  DealsExcessDamage identityMatcher -> case windowType w of
    DefeatedMinion _ damage source n
      | windowTiming w == After && n > 0 -> damageMatchesFrom (DamageByPlayer identityMatcher) damage source
    DefeatedVillain _ damage source n
      | windowTiming w == After && n > 0 -> damageMatchesFrom (DamageByPlayer identityMatcher) damage source
    _ -> pure False
  AllyDefends allyMatcher -> case windowType w of
    AllyDefendedAgainst allyId _
      | windowTiming w == After -> allyMatches allyMatcher allyId
    _ -> pure False
  PlayerCardWouldBeDiscardedFromPlay -> case windowType w of
    WouldBeDiscardedFromPlay (Left _) -> pure $ windowTiming w == Would
    _ -> pure False
  CardWithIdWouldBeDiscardedFromPlay cid -> case windowType w of
    WouldBeDiscardedFromPlay card -> pure $ windowTiming w == Would && either toCardId toCardId card == cid
    _ -> pure False
  CardWouldBeDiscardedFromPlay -> case windowType w of
    WouldBeDiscardedFromPlay _ -> pure $ windowTiming w == Would
    _ -> pure False
  MakesBasicDefense timing identityMatcher enemyMatcher -> case windowType w of
    MadeBasicDefense identityId enemyId
      | windowTiming w == timing ->
          liftA2
            (&&)
            (identityMatches identityMatcher identityId)
            (if enemyMatcher == AnyEnemy then pure True else enemyMatches enemyMatcher enemyId)
    _ -> pure False
  HeroDefended timing identityMatcher enemyMatcher -> case windowType w of
    HeroDefends identityId enemyId
      | windowTiming w == timing ->
          liftA2
            (&&)
            (identityMatches identityMatcher identityId)
            (enemyMatches enemyMatcher enemyId)
    _ -> pure False
  IdentityWouldBeDefeatedByEnemyAttack matcher -> case windowType w of
    IdentityDefeatFromDamage ident source damage
      | windowTiming w == Would
      , damageSource damage `elem` [FromAttack, FromOverkill]
      , Just _ <- sourceEnemy source -> identityMatches matcher ident
    _ -> pure False
  IdentityWouldBeDefeated identityMatcher -> case windowType w of
    IdentityDefeat identityId
      | windowTiming w == Would ->
          identityMatches identityMatcher identityId
    _ -> pure False
  TurnBeginsAt timing identityMatcher -> case windowType w of
    TurnBegan ident | windowTiming w == timing -> identityMatches identityMatcher ident
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
  PlaysEventPaidWith timing identityMatcher cardMatcher resource -> case windowType w of
    EventPlayed ident eid def
      | windowTiming w == timing && cardMatch cardMatcher def -> do
          yours <- identityMatches identityMatcher ident
          spent <- getEventPaidResources eid
          pure $ yours && resource `elem` spent
    _ -> pure False
  PlaysEvent timing identityMatcher cardMatcher -> case windowType w of
    EventPlayed ident _ def
      | windowTiming w == timing && cardMatch cardMatcher def ->
          identityMatches identityMatcher ident
    _ -> pure False
  AllyDefeated timing allyMatcher -> case windowType w of
    DefeatedAlly allyId _
      | windowTiming w == timing -> allyMatches allyMatcher allyId
    _ -> pure False
  AllyDefeatedByAttack timing allyMatcher -> case windowType w of
    DefeatedAlly allyId (Just _)
      | windowTiming w == timing -> allyMatches allyMatcher allyId
    _ -> pure False
  AllyDefeatedByAttackOfEnemy timing allyMatcher enemyMatcher -> case windowType w of
    DefeatedAlly aid (Just enemy) | windowTiming w == timing -> (&&) <$> allyMatches allyMatcher aid <*> enemyMatches enemyMatcher enemy
    _ -> pure False
  AllyDefeatedByAttackOf timing enemyMatcher -> case windowType w of
    DefeatedAlly _ (Just enemyId)
      | windowTiming w == timing -> enemyMatches enemyMatcher enemyId
    _ -> pure False
  BoostCardFlipped timing enemyMatcher -> case windowType w of
    FlippedBoostCard _ enemyId
      | windowTiming w == timing -> enemyMatches enemyMatcher enemyId
    _ -> pure False
  EncounterCardRevealedFromDeckBy timing identityMatcher encounterCardMatcher -> case windowType w of
    EncounterCardRevealed RevealedFromEncounterDeck ident encounterCard | windowTiming w == timing ->
      (&&) <$> encounterCardMatches encounterCardMatcher encounterCard <*> identityMatches identityMatcher ident
    _ -> pure False
  EncounterCardRevealedBy timing identityMatcher encounterCardMatcher -> case windowType w of
    EncounterCardResolved ident card | timing == After -> (&&) <$> identityMatches identityMatcher ident <*> encounterCardMatches encounterCardMatcher card
    EncounterCardRevealed _ ident encounterCard
      | windowTiming w == timing ->
          (&&) <$> encounterCardMatches encounterCardMatcher encounterCard <*> identityMatches identityMatcher ident
    _ -> pure False
  PlaysCard timing identityMatcher cardMatcher -> case windowType w of
    CardPlayed ident card
      | windowTiming w == timing && cardMatch cardMatcher card ->
          identityMatches identityMatcher ident
    _ -> pure False
  PlaysUpgradeOnAlly timing identityMatcher -> case windowType w of
    UpgradePlayedOnAlly ident _ _ | windowTiming w == timing -> identityMatches identityMatcher ident
    _ -> pure False
  UsesAbility timing identityMatcher index -> case windowType w of
    UsedCardAbility ident source usedIndex | source == IdentityRef ident && windowTiming w == timing && index == usedIndex -> identityMatches identityMatcher ident
    _ -> pure False
  AllyEntersPlay timing allyMatcher -> case windowType w of
    AllyEnteredPlay aid | windowTiming w == timing -> allyMatches allyMatcher aid
    _ -> pure False
  PlaysAlly allyMatcher -> case windowType w of
    PlayedAlly aid | windowTiming w == After -> allyMatches allyMatcher aid
    _ -> pure False
  PlayedFromHand identityMatcher cardMatcher -> case windowType w of
    PlayedCardFromHand ident card | windowTiming w == After && cardMatch cardMatcher card -> identityMatches identityMatcher ident
    _ -> pure False
  PlaysCardFromHand identityMatcher cardMatcher -> case windowType w of
    PlayingCardFromHand ident card | cardMatch cardMatcher card -> identityMatches identityMatcher ident
    _ -> pure False
  AllyLeavesPlay timing identityMatcher cardMatcher -> case windowType w of
    AllyLeavingPlay ident _ def
      | windowTiming w == timing && cardMatch cardMatcher def -> identityMatches identityMatcher ident
    _ -> pure False
  ResolvesEventAbility timing identityMatcher cardMatcher -> case windowType w of
    EventAbilityResolved ident _ def
      | windowTiming w == timing && cardMatch cardMatcher def -> identityMatches identityMatcher ident
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
  AllyTookDamageOf allyMatcher damageSource' -> case windowType w of
    AllyTookDamage aid _ damage
      | windowTiming w == After && damageAmount damage > 0
      , damageSource' == FromAnyDamageSource || damageSource damage == damageSource' -> allyMatches allyMatcher aid
    _ -> pure False
  MutantChangesToHero timing identityMatcher -> case windowType w of
    MutantChangedToHero ident | windowTiming w == timing -> identityMatches identityMatcher ident
    _ -> pure False
  AllyDefeatedByConsequentialDamage timing identityMatcher -> case windowType w of
    AllyCardDefeatedByConsequentialDamage ident _ | windowTiming w == timing -> identityMatches identityMatcher ident
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

-- | Whether the villain would take the damage his "when dealt damage" window is for: some
-- (none is opened with damage he cannot take, as 0), and not all of it prevented by his
-- tough status card, whose interrupt is first (RR Simultaneous Timing Priority).
wouldTakeDamage :: (HasGame m) => VillainId -> Damage -> m Bool
wouldTakeDamage villainId damage
  | damageAmount damage == 0 = pure False
  | preventedByTough damage = not <$> villainMatches VillainWithToughStatus villainId
  | otherwise = pure True
