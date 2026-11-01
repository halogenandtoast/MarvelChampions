{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Minion.Runner (
  module Marvel.Minion.Runner,
  module X,
) where

import Marvel.Prelude
import Marvel.Message (recordPlayerAttackDamage)

import Marvel.Card.Code as X
import Marvel.Entity as X
import Marvel.Hp as X
import Marvel.Id as X
import Marvel.Message.Lifted as X
import Marvel.Minion.Types as X hiding (Field (..))
import Marvel.Modifier as X
import Marvel.Query as X
import Marvel.Question as X
import Marvel.Ref as X
import Marvel.Stats as X

import Data.HashSet qualified as HashSet
import Marvel.Attack
import Marvel.Boost
import Marvel.Card
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Window qualified as W

statusL :: StatusCard -> Lens' (Attrs Minion) Bool
statusL = \case
  StunnedStatus -> stunnedL
  ConfusedStatus -> confusedL
  ToughStatus -> toughL

-- | The status card is given, a stunned or confused one not to a stalwart minion (RR
-- Stalwart).
statusUnlessStalwart :: (HasGame m) => Attrs Minion -> StatusCard -> m (Attrs Minion)
statusUnlessStalwart attrs s = do
  keywords <- getModifiedKeywords attrs
  modifiers <- getModifiers attrs
  pure $ if (Stalwart `elem` keywords && s /= ToughStatus) || (s == StunnedStatus && CannotBeStunned `elem` modifiers) || (s == ConfusedStatus && CannotBeConfused `elem` modifiers) then attrs else
    case s of
      ToughStatus | CanHaveAnyNumberOfToughStatusCards `elem` modifiers && minionTough attrs -> attrs {minionAdditionalTough = minionAdditionalTough attrs + 1}
      StunnedStatus | Steady `elem` keywords && minionStunned attrs -> attrs {minionAdditionalStun = True}
      ConfusedStatus | Steady `elem` keywords && minionConfused attrs -> attrs {minionAdditionalConfuse = True}
      _ -> attrs & statusL s .~ True

removeOneTough :: Attrs Minion -> Attrs Minion
removeOneTough attrs
  | minionAdditionalTough attrs > 0 = attrs {minionAdditionalTough = minionAdditionalTough attrs - 1}
  | otherwise = attrs & toughL .~ False

runMinionMessage ::
  (ReverseQueue m) => MinionMessage -> Attrs Minion -> m (Attrs Minion)
runMinionMessage msg attrs = case msg of
  MinionHealed n -> do
    pure $ attrs & damageL %~ subtractNatural n
  MinionCheckHitPoints -> do
    hitPoints <- getModifiedHitPoints attrs
    blocked <- elem CannotBeDefeated <$> getModifiers attrs
    when (not blocked && minionDamage attrs >= hitPoints) $ defeatMinion (minionId attrs)
    pure attrs
  MinionHealAllDamage -> do
    pure $ attrs & damageL .~ 0
  MinionDamaged source originalDamage -> do
    modifiers <- getModifiers attrs
    damage <- incomingMinionDamage attrs source originalDamage
    hitPoints <- getModifiedHitPoints attrs
    cannotTakeDamage <- cannotTakeDamageFrom (toRef attrs) source
    if
      -- already defeated: damage to a minion on its way out of play does nothing
      | minionDamage attrs >= hitPoints && CannotBeDefeated `notElem` modifiers -> pure attrs
      | cannotTakeDamage -> pure attrs
      | minionTough attrs && preventedByTough originalDamage -> pure $ removeOneTough attrs
      | otherwise -> do
        when (damageAmount damage > 0) $ do
          recordPlayerAttackDamage (toEnemyId attrs) source damage
          checkWindows [W.Window W.After $ W.EnemyTookDamageFrom (toEnemyId attrs) damage source]
        when (damageAmount damage + minionDamage attrs >= hitPoints && CannotBeDefeated `notElem` modifiers) do
          let excess = subtractNatural hitPoints (damageAmount damage + minionDamage attrs)
          -- "when your hero's attack deals excess damage, increase that amount"
          hero <- heroAttackBy damage source
          for_ hero \ident ->
            when (excess > 0) $ checkWindows [W.Window W.When $ W.ExcessDamageDealt ident (toEnemyId attrs) excess]
          minionStep (minionId attrs) (MinionDefeatedWithExcess source damage excess)
        pure $ (attrs & damageL +~ damageAmount damage & toughL %~ (&& not (piercesTough damage))) {minionDamageThisPhase = minionDamageThisPhase attrs + damageAmount damage, minionAdditionalTough = if piercesTough damage then 0 else minionAdditionalTough attrs}
  MinionDefeatedWithExcess source damage overkill -> do
    -- overkill follows the defeat: a minion that is not defeated after all deals none
    -- past it ('cancelMinionDefeat')
    blocked <- elem CannotBeDefeated <$> getModifiers attrs
    unless blocked do
      attached <- enemyDefeatUpgrades (toEnemyId attrs)
      checkWindows [W.Window W.When $ W.DefeatedMinion (minionId attrs) damage source overkill, W.Window W.When $ W.DefeatedEnemyWithUpgrades (toEnemyId attrs) attached damage source]
      defeatMinion (minionId attrs)
      when (overkill > 0 && damageOverkill damage) do
        traverse_ (\villain -> damageCharacter villain source (toDamage overkill FromOverkill)) =<< selectOne ActiveVillain
      checkWindows [W.Window W.After $ W.DefeatedMinion (minionId attrs) damage source overkill, W.Window W.After $ W.DefeatedEnemyWithUpgrades (toEnemyId attrs) attached damage source]
    pure attrs
  -- retaliate answers the attack whether or not a tough status card took its damage
  MinionRetaliates attacker -> do
    keywords <- getModifiedKeywords attrs
    for_ [n | Retaliate n <- toList keywords] (retaliate attrs attacker)
    pure attrs
  -- a stalwart minion cannot be stunned or confused (RR Stalwart)
  MinionStunned _ -> statusUnlessStalwart attrs StunnedStatus
  MinionConfused _ -> statusUnlessStalwart attrs ConfusedStatus
  MinionBecomeTough -> statusUnlessStalwart attrs ToughStatus
  MinionDefendedBy characterId -> do
    forbidden <- elem AttacksCannotBeDefended <$> getModifiers attrs
    pure $ if forbidden then attrs else attrs & attackingL . _Just %~ defendedBy characterId
  AttachedToMinion attachmentId -> do
    pure $ attrs & attachmentsL %~ HashSet.insert attachmentId
  AttachedUpgradeToMinion upgradeId ->
    pure $ attrs & upgradesL %~ HashSet.insert upgradeId
  RevealMinion _ -> pure attrs
  -- quickstrike, as a forced ability, attacks before the window for engaging opens
  MinionEngagedIdentity ident -> do
    isHero <- identityMatches HeroIdentity ident
    let previous = minionEngagedIdentity attrs
    when (previous /= ident) $ minionDisengages previous (minionId attrs)
    keywords <- getModifiedKeywords attrs
    when (isHero && Quickstrike `elem` keywords) $ enemyActivationAfterCurrent (minionId attrs) (enemyAttacks (minionId attrs) ident)
    checkWindows [W.Window W.After $ W.EngagedMinion ident (minionId attrs)]
    pure attrs {minionEngagedIdentity = ident}
  -- with victory X, it and an attachment on it with victory X go to the victory display
  MinionDefeated -> do
    -- A printed When Defeated effect and its interrupts have resolved before
    -- this base step; the minion still has all its attachments and status cards.
    checkWindows [W.Window W.Would $ W.MinionLeavingPlay (minionId attrs)]
    resolveMinionLeavingPlay (minionId attrs)
    pure attrs
  MinionLeavesPlay -> do
    -- The departure handles attached cards after the host's discard interrupts.
    defeatedLeavesPlay attrs
    minionDisengages (minionEngagedIdentity attrs) (minionId attrs)
    pure attrs
  -- a villainous minion is given a boost card as it uses a basic power (RR, Villainous)
  MinionSchemesAgainst you -> do
    minionStep (minionId attrs) MinionSchemes
    minionStep (minionId attrs) MinionSchemeAgainstEnded
    pure attrs {minionActivationPlayer = Just you}
  MinionSchemeAgainstEnded -> pure attrs {minionActivationPlayer = Nothing}
  MinionSchemes -> do
    forbidden <- elem CannotActivate <$> getModifiers attrs
    confused <- isConfused attrs
    if forbidden then pure attrs else if confused
      then pure $ attrs {minionAdditionalConfuse = False} & confusedL .~ False
      else do
        villainous <- isVillainous
        -- the scheme is its activation against the player it is engaged with
        let activation timing = checkWindows [W.Window timing $ W.EnemyActivation (toEnemyId attrs) (fromMaybe (minionEngagedIdentity attrs) $ minionActivationPlayer attrs)]
        activation W.When
        when villainous do
          dealBoost attrs
          minionStep (minionId attrs) MinionFlipBoostCards
        minionSchemed (minionId attrs)
        when villainous clearBoosts
        activation W.After
        pure attrs
  -- "attacks an ally": the player who controls it is attacked
  MinionAttacksAlly ident allyId -> do
    enemyAttacks (minionId attrs) ident
    pure attrs {minionAttackTarget = Just (AllyCharacter allyId)}
  -- a stunned minion discards its stun instead (RR Stun)
  MinionAttacksEnemy enemy -> do
    forbidden <- elem CannotActivate <$> getModifiers attrs
    stunned <- isStunned attrs
    if forbidden then pure attrs else if stunned
      then pure $ attrs {minionAdditionalStun = False} & stunnedL .~ False
      else do
        checkWindows [W.Window W.When $ W.EnemyAttackOnEnemy (toEnemyId attrs) enemy]
        minionStep (minionId attrs) (MinionAttackEnemyAfterInterrupt enemy)
        pure attrs
  MinionAttackEnemyAfterInterrupt enemy -> do
    present <- selectAny (MinionWithId $ minionId attrs)
    targetPresent <- selectAny (EnemyWithId enemy)
    when (present && targetPresent) do
      keywords <- getModifiedKeywords attrs
      atk <- getModifiedAttack attrs
      let attack' = attackWith keywords (toEnemyId attrs) (MinionCharacter $ minionId attrs) atk
      dealDamage enemy attrs (attackDamageTo attack' atk)
      unless (attackRanged attack') $ enemyRetaliates enemy (MinionCharacter $ minionId attrs)
      checkWindows [W.Window W.After $ W.EnemyAttackOnEnemy (toEnemyId attrs) enemy]
    pure attrs
  MinionAttacksWithChange ident n keywords -> do
    updated <- runMinionMessage (MinionAttacks ident) attrs
    pure updated {minionAttacking = fmap (\a -> (foldr gainKeyword a keywords) {attackChange = attackChange a + n}) (minionAttacking updated)}
  MinionAttacks ident -> do
    forbidden <- elem CannotActivate <$> getModifiers attrs
    stunned <- isStunned attrs
    if forbidden then pure attrs else if stunned
      then pure $ attrs {minionAdditionalStun = False} & stunnedL .~ False & \a -> a {minionAttackTarget = Nothing}
      else do
        -- the attack is its activation against the player, from start to end, whether
        -- the attack resolves or is cancelled (a stunned minion does not activate)
        checkWindows [W.Window W.When $ W.EnemyActivation (toEnemyId attrs) ident]
        checkWindows [attackWindow W.Would ident]
        minionBeginsAttack (minionId attrs) ident
        minionEndsAttack (minionId attrs)
        whenM isVillainous clearBoosts
        checkWindows [W.Window W.After $ W.EnemyActivation (toEnemyId attrs) ident]
        -- the attack is under way from when it is declared, as its would windows open
        attack' <- newAttack ident
        pure $ attrs & attackingL ?~ attack'
  MinionBeginAttack ident -> do
    attack' <- keepingGains (minionAttacking attrs) <$> newAttack ident
    villainous <- isVillainous
    checkWindows [attackWindow W.When ident]
    when villainous $ dealBoost attrs
    declareDefense ident (toEnemyId attrs) (minionDefensePriority attrs)
    when villainous $ minionStep (minionId attrs) MinionFlipBoostCards
    minionAttacked (minionId attrs)
    checkWindows [attackWindow W.After ident]
    pure $
      attrs {minionAttackTarget = Nothing} & attackingL ?~ attack'
  -- onto the main scheme of its area, if there is one there (as between The Once and
  -- Future Kang's stages): of two, the active villain's own, which Focused Defense is on
  -- (Tower Defense, RR v1.8 p.67 errata)
  MinionSchemed -> do
    paired <- maybe (pure Nothing) getPairedMainScheme =<< selectOne ActiveVillain
    mainScheme <- maybe (selectOne EncounterMainScheme) (pure . Just . SchemeMainSchemeId) paired
    threat <- getModifiedScheme attrs
    for_ [m | Just (SchemeMainSchemeId m) <- [mainScheme]] \mainSchemeId ->
      placeThreatFrom W.ThreatFromMinion mainSchemeId (threat + minionBoost attrs)
    checkWindows [W.Window W.After $ W.SchemedByMinion (minionId attrs)]
    pure $ attrs & boostL .~ 0
  MinionEndAttack -> pure $ attrs & attackingL .~ Nothing
  MinionAttackGains k -> pure $ attrs & attackingL . _Just %~ gainKeyword k
  MinionHoldsCard card -> pure attrs {minionHeldCards = minionHeldCards attrs <> [card]}
  MinionAttackBecomesDividedEvenly -> pure $ attrs & attackingL . _Just %~ \a -> a {attackDividedEvenly = True}
  MinionAttackDamageRedirected enemy -> pure $ attrs & attackingL . _Just %~ (\a -> a {attackDamageRedirect = Just enemy})
  MinionAttackBecomesIndirect -> pure $ attrs & attackingL . _Just %~ \a -> a {attackIndirect = True}
  MinionAttackChanged n -> pure $ attrs & attackingL . _Just %~ \a -> a {attackChange = attackChange a + n}
  MinionAttackDamagePrevented -> pure $ attrs & attackingL . _Just %~ \a -> a {attackPrevented = True}
  MinionAttacked -> do
    case minionAttacking attrs of
      Nothing -> error "No current attack"
      Just attack0 -> do
        actual <- undefendedIfDefenderGone attack0
        base <- getModifiedAttackSigned (attrs {minionAttacking = Just actual})
        let refreshed = withAttackBase (base + toInteger (minionBoost attrs)) actual
        attack' <- undefendedIfDefenderGone refreshed
        attackedYou (attackEnemy attack') (attackCharacter attack')
        wasAttacked attack'
        pure $ attrs & boostL .~ 0 & attackingL ?~ attack'
  MinionDealtBoost card -> pure $ attrs & boostCardsL %~ (<> [card])
  -- each boost card is turned faceup in turn, and its icons added; one it is given as
  -- they are ("give this enemy another boost card") is turned up after them
  MinionFlipBoostCards -> do
    for_ (minionBoostCards attrs) \card -> do
      checkWindows [W.Window W.When $ W.FlippedBoostCard card (toEnemyId attrs)]
      ignored <- elem IgnoreBoostCards <$> getModifiers attrs
      if ignored
        then revealBoostCardWithoutAbility card (minionId attrs)
        else do
          revealBoostCard card (minionId attrs)
          countActivationBoostIcons attrs [card]
    unless (null $ minionBoostCards attrs) $ minionStep (minionId attrs) MinionFlipBoostCards
    pure $ attrs & boostCardsL .~ []
  MinionAddBoost n -> pure $ attrs & boostL +~ n & attackingL . _Just . attackDamageL +~ n
 where
  isVillainous = elem Villainous <$> getModifiedKeywords attrs
  attackTarget ident = fromMaybe (IdentityCharacter ident) (minionAttackTarget attrs)
  attackWindow timing ident = W.Window timing $ case minionAttackTarget attrs of
    Just (AllyCharacter aid) -> W.EnemyAttackAlly (toEnemyId attrs) aid ident
    _ -> W.EnemyAttack (toEnemyId attrs) ident
  newAttack ident = do
    base <- getModifiedAttackSigned attrs
    keywords <- getModifiedKeywords attrs
    pure $ withAttackBase base $ attackWith keywords (toEnemyId attrs) (attackTarget ident) 0

instance RunMessage (Attrs Minion) where
  runMessage msg attrs = runQueueT do
    updated <- case msg of
      CardSpecific target "neRedirectAttackToAlly" (fromJSON -> Success aid) | target == toRef attrs ->
        pure attrs {minionAttackTarget = Just (AllyCharacter aid), minionAttacking = fmap (\a -> a {attackCharacter = AllyCharacter aid, attackTarget = Nothing}) (minionAttacking attrs)}
      RemovePlayerCardFromHeldZones cid -> pure attrs {minionHeldCards = filter ((/= cid) . pcCardId) (minionHeldCards attrs)}
      BeginPhase _ -> pure attrs {minionDamageThisPhase = 0}
      RanAbility _ (isTarget attrs -> True) 9901 _ _ -> do
        let ident = minionEngagedIdentity attrs
        present <- selectAny (MinionWithId $ minionId attrs)
        hero <- identityMatches HeroIdentity ident
        when present $ enemyActivationAfterCurrent (minionId attrs) $
          if hero then enemyAttacks (minionId attrs) ident else minionSchemes (minionId attrs)
        pure attrs
      GiveStatus (MinionRef mid) s | mid == minionId attrs -> statusUnlessStalwart attrs s
      RemoveStatus (MinionRef mid) StunnedStatus | mid == minionId attrs, minionAdditionalStun attrs -> pure attrs {minionAdditionalStun = False}
      RemoveStatus (MinionRef mid) ConfusedStatus | mid == minionId attrs, minionAdditionalConfuse attrs -> pure attrs {minionAdditionalConfuse = False}
      RemoveStatus (MinionRef mid) ToughStatus | mid == minionId attrs -> pure $ removeOneTough attrs
      RemoveStatus (MinionRef mid) s | mid == minionId attrs -> pure $ attrs & statusL s .~ False
      MinionMessage ident msg'
        | ident == minionId attrs ->
            runMinionMessage msg' attrs
      AttachmentRemoved attachmentId ->
        pure $ attrs & attachmentsL %~ HashSet.delete attachmentId
      UpgradeMessage uid UpgradeDetached -> pure $ attrs & upgradesL %~ HashSet.delete uid
      UpgradeRemoved upgradeId -> pure $ attrs & upgradesL %~ HashSet.delete upgradeId
      WithBoostIcons (MinionRef mid) _ counts | mid == minionId attrs -> do
        minionStep mid (MinionAddBoost $ sum $ map snd counts)
        pure attrs
      _ -> pure attrs
    if not (minionAdditionalStun updated || minionAdditionalConfuse updated)
      then pure updated
      else do
        steady <- elem Steady <$> getModifiedKeywords updated
        pure $ if steady then updated else updated {minionAdditionalStun = False, minionAdditionalConfuse = False}

-- Hela

-- | "<this minion> engages the first player" (Garm, Skurge, Nidhogg): a constant
-- ability. Engaged with anyone else, it engages the first player; it follows the first
-- player token as it passes, or as the first player is chosen or eliminated. A card's
-- handler calls it on each message before falling through.
engagesFirstPlayer :: (ReverseQueue m) => Message -> Attrs Minion -> m ()
engagesFirstPlayer msg attrs = case msg of
  MinionMessage mid (MinionEngagedIdentity _) | mid == minionId attrs -> follow
  PassFirstPlayer -> follow
  SetPlayerOrder _ -> follow
  RemoveFromPlay (IdentityRef _) -> follow
  CardSpecific (isTarget attrs -> True) "engageFirstPlayer" _ -> do
    let mid = minionId attrs
    inPlay <- selectAny (MinionWithId mid)
    engaged <- selectAny (MinionWithId mid <> MinionEngagedWith FirstPlayer)
    when (inPlay && not engaged) $ selectOne FirstPlayer >>= traverse_ (engageMinion mid)
  _ -> pure ()
 where
  -- once the game has seen to the engagement or the player order
  follow = cardSpecific attrs "engageFirstPlayer" ()

-- | Hit points printed per player: these many more for each player past the first
-- (RR Per Player Icon: the players who started the scenario).
perPlayerHitPoints :: (HasGame m) => Attrs Minion -> Natural -> m [Modifier]
perPlayerHitPoints _ n = do
  players <- getStartingPlayerCount
  pure [HitPointModifier (n * fromIntegral (players - 1)) | players > 1]
