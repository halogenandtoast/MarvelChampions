{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Minion.Runner (
  module Marvel.Minion.Runner,
  module X,
) where

import Marvel.Prelude
import Marvel.Message ()

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
import Marvel.Boost ()
import Marvel.Card
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Trait (Trait(Elite))
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

flipMinionBoostCards :: ReverseQueue m => Attrs Minion -> m (Attrs Minion)
flipMinionBoostCards attrs = do
  for_ (minionBoostCards attrs) \card -> do
    ignored <- elem IgnoreBoostCards <$> getModifiers attrs
    if ignored
      then revealBoostCardWithoutAbility card (minionId attrs)
      else do
        revealBoostCard card (minionId attrs)
        countActivationBoostIcons attrs [card]
  unless (null $ minionBoostCards attrs) $ minionStep (minionId attrs) MinionFlipBoostCards
  pure $ attrs & boostCardsL .~ []

runMinionMessage ::
  (ReverseQueue m) => MinionMessage -> Attrs Minion -> m (Attrs Minion)
runMinionMessage msg attrs = case msg of
  MinionSchemes | not (minionSchemeAvailable attrs) -> pure attrs
  MinionAttacks _ | not (minionAttackAvailable attrs) -> pure attrs
  MinionAttacksWithChange _ _ _ | not (minionAttackAvailable attrs) -> pure attrs
  MinionAttacksAlly _ _ | not (minionAttackAvailable attrs) -> pure attrs
  MinionAttacksEnemy _ | not (minionAttackAvailable attrs) -> pure attrs
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
    forbidden <- not <$> characterAttackDamageAllowed (toRef attrs) originalDamage
    cannotTakeDamage <- cannotTakeDamageFrom (toRef attrs) source
    if
      -- already defeated: damage to a minion on its way out of play does nothing
      | minionDamage attrs >= hitPoints && CannotBeDefeated `notElem` modifiers -> pure attrs
      | (cannotTakeDamage || forbidden) && not (damageUnpreventable originalDamage) -> pure attrs
      | minionTough attrs && preventedByTough originalDamage -> pure $ removeOneTough attrs
      | otherwise -> do
        let excess = subtractNatural hitPoints (damageAmount damage + minionDamage attrs)
        when (excess > 0) $ do
          checkWindows [W.Window W.When $ W.CharacterTookExcessDamage (MinionCharacter $ minionId attrs) source excess]
          for_ (sourceEnemy source) $ \enemy -> when ((damageSource damage `elem` [FromAttack,FromOverkill])) $ cardSpecific (enemyRef enemy) "attackExcessDamage" excess
        when (damageAmount damage > 0) $ do
          recordPlayerAttackDamage (toEnemyId attrs) source damage
        when (damageAmount damage + minionDamage attrs >= hitPoints && CannotBeDefeated `notElem` modifiers) do
          let excess = subtractNatural hitPoints (damageAmount damage + minionDamage attrs)
          -- "when your hero's attack deals excess damage, increase that amount"
          hero <- heroAttackBy damage source
          for_ hero \ident ->
            when (excess > 0) $ checkWindows [W.Window W.When $ W.ExcessDamageDealt ident (toEnemyId attrs) excess]
          minionStep (minionId attrs) (MinionDefeatedWithExcess source damage excess)
        -- Damage responses follow defeat and departure (RR Damage, step 9).
        when (damageAmount damage > 0) do
          attackDealtDamageTo source damage (minionId attrs)
          checkWindows [W.Window W.After $ W.EnemyTookDamageFrom (toEnemyId attrs) damage source]
        pure $ (attrs & damageL +~ damageAmount damage & toughL %~ (&& not (piercesTough damage))) {minionDamageThisPhase = minionDamageThisPhase attrs + damageAmount damage, minionAdditionalTough = if piercesTough damage then 0 else minionAdditionalTough attrs}
  MinionDefeatedWithExcess source damage overkill -> do
    baseScheme <- minionBaseScheme attrs
    -- overkill follows the defeat: a minion that is not defeated after all deals none
    -- past it ('cancelMinionDefeat')
    blocked <- elem CannotBeDefeated <$> getModifiers attrs
    unless blocked do
      attached <- enemyDefeatUpgrades (toEnemyId attrs)
      checkWindows [W.Window W.When $ W.DefeatedMinion (minionId attrs) damage source overkill, W.Window W.When $ W.DefeatedEnemyWithUpgrades (toEnemyId attrs) attached damage source]
      minionStep (minionId attrs) MinionDefeated
      when (overkill > 0 && damageOverkill damage) do
        traverse_ (\villain -> damageCharacter villain source (toDamage overkill FromOverkill)) =<< selectOne ActiveVillain
      checkWindows [W.Window W.After $ W.DefeatedMinion (minionId attrs) damage source overkill, W.Window W.After $ W.DefeatedEnemyWithUpgrades (toEnemyId attrs) attached damage source]
    pure attrs {minionDefeatedBaseScheme = if blocked then Nothing else Just baseScheme}
  -- retaliate answers the attack whether or not a tough status card took its damage
  MinionRetaliates attacker -> do
    keywords <- getModifiedKeywords attrs
    let amount = sum [n | Retaliate n <- keywords]
    when (amount > 0) $ retaliate attrs attacker amount
    pure attrs
  -- a stalwart minion cannot be stunned or confused (RR Stalwart)
  MinionStunned _ -> applyVulnerableStatus attrs StunnedStatus
  MinionConfused _ -> applyVulnerableStatus attrs ConfusedStatus
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
    modifiers <- getModifiers attrs
    when (isHero && (Quickstrike `elem` keywords || QuickstrikeWhenEngaging ident `elem` modifiers)) $
      enemyActivationAfterCurrent (minionId attrs) (enemyAttacks (minionId attrs) ident)
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
    -- Refresh after When Defeated and would-leave interrupts, while the host and
    -- all its constant modifiers are still present.
    baseScheme <- minionBaseScheme attrs
    -- The departure handles attached cards after the host's discard interrupts.
    defeatedLeavesPlay attrs
    minionDisengages (minionEngagedIdentity attrs) (minionId attrs)
    pure attrs {minionDefeatedBaseScheme = Just baseScheme}
  -- a villainous minion is given a boost card as it uses a basic power (RR, Villainous)
  MinionSchemesAgainst you -> do
    minionStep (minionId attrs) MinionSchemes
    minionStep (minionId attrs) MinionSchemeAgainstEnded
    pure attrs {minionActivationPlayer = Just you}
  MinionSchemeAgainstEnded -> pure attrs {minionActivationPlayer = Nothing, minionSchemeReplacesAttack = False,minionBoost = 0}
  MinionSchemes -> do
    forbidden <- elem CannotActivate <$> getModifiers attrs
    confused <- isConfused attrs
    if forbidden then pure attrs {minionBoost = 0} else if confused
      then pure $ attrs {minionAdditionalConfuse = False,minionBoost = 0} & confusedL .~ False
      else do
        let you = fromMaybe (minionEngagedIdentity attrs) (minionActivationPlayer attrs)
        checkWindows [W.Window W.Would $ W.EnemySchemeBegins (toEnemyId attrs) you]
        minionStep (minionId attrs) MinionBeginScheme
        pure attrs
  MinionBeginScheme -> do
    villainous <- isVillainous
    -- the scheme is its activation against the player it is engaged with
    let activation timing = when (timing /= W.When || not (minionSchemeReplacesAttack attrs)) $ checkWindows [W.Window timing $ W.EnemyActivation (toEnemyId attrs) (fromMaybe (minionEngagedIdentity attrs) $ minionActivationPlayer attrs)]
    activation W.When
    checkWindows [W.Window W.When $ W.SchemedByMinion (minionId attrs)]
    suppressBoost <- elem NoActivationBoostCard <$> getModifiers attrs
    when (villainous && not suppressBoost) $ dealBoost attrs
    minionStep (minionId attrs) MinionFlipBoostCards
    minionSchemed (minionId attrs)
    clearBoosts
    activation W.After
    pure attrs
  -- "attacks an ally": the player who controls it is attacked
  MinionAttacksAlly ident allyId -> do
    enemyAttacks (minionId attrs) ident
    pure attrs {minionAttackTarget = Just (AllyCharacter allyId)}
  -- a stunned minion discards its stun instead (RR Stun)
  MinionAttacksEnemy enemy -> do
    present <- inAnyArea $ selectAny (MinionWithId $ minionId attrs)
    targetPresent <- inAnyArea $ selectAny (EnemyWithId enemy)
    when (present && targetPresent) $ do
      you <- getActivePlayerId
      minionStep (minionId attrs) (MinionAttacks you)
    pure $ if present && targetPresent then attrs {minionAttackTarget = Just (toCharacter enemy)} else attrs
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
    if forbidden then pure attrs {minionAttackTarget = Nothing,minionBoost = 0} else if stunned
      then pure $ attrs {minionAdditionalStun = False,minionBoost = 0} & stunnedL .~ False & \a -> a {minionAttackTarget = Nothing}
      else do
        -- the attack is its activation against the player, from start to end, whether
        -- the attack resolves or is cancelled (a stunned minion does not activate)
        checkWindows [W.Window W.When $ W.EnemyActivation (toEnemyId attrs) ident]
        checkWindows [attackWindow W.Would ident]
        minionBeginsAttack (minionId attrs) ident
        minionEndsAttack (minionId attrs)
        clearBoosts
        checkWindows [W.Window W.After $ W.EnemyActivation (toEnemyId attrs) ident]
        -- the attack is under way from when it is declared, as its would windows open
        attack' <- newAttack ident
        pure $ attrs {minionActivationPlayer = Just ident} & attackingL ?~ attack'
  MinionBeginAttack ident -> do
    attack' <- keepingGains (minionAttacking attrs) <$> newAttack ident
    villainous <- isVillainous
    checkWindows [attackWindow W.When ident]
    suppressBoost <- elem NoActivationBoostCard <$> getModifiers attrs
    when (villainous && not suppressBoost) $ dealBoost attrs
    case attackTarget ident of
      VillainCharacter _ -> pure ()
      MinionCharacter _ -> pure ()
      AttachmentCharacter _ -> pure ()
      _ -> declareDefense ident (toEnemyId attrs) (minionDefensePriority attrs)
    minionStep (minionId attrs) MinionFlipBoostCards
    minionAttacked (minionId attrs)
    checkWindows [attackWindow W.After ident]
    pure $
      attrs {minionAttackTarget = Nothing} & attackingL ?~ attack'
  -- onto the main scheme of its area, if there is one there (as between The Once and
  -- Future Kang's stages): of two, the active villain's own, which Focused Defense is on
  -- (Tower Defense, RR v1.8 p.67 errata)
  MinionSchemed -> do
    owned <- getOwnedMainScheme $ minionEngagedIdentity attrs
    paired <- maybe (pure Nothing) getPairedMainScheme =<< selectOne ActiveVillain
    mainScheme <- maybe (selectOne EncounterMainScheme) (pure . Just . SchemeMainSchemeId) (owned <|> paired)
    threat <- getModifiedScheme attrs
    modifiers <- getModifiers attrs
    destinations <- filterM (selectAny . SideSchemeWithId) [sid | MinionSchemesOn sid <- modifiers]
    case (minionSchemeRemoval attrs, headMay destinations) of
      (Just you, _) -> for_ mainScheme $ \scheme -> cardSpecific GameRef "aosInformantScheme" (you,toSource attrs,scheme,threat + minionBoost attrs)
      (Nothing, Just sid) -> placeThreatBy attrs W.ThreatFromMinion sid (threat + minionBoost attrs)
      (Nothing, Nothing) -> for_ [m | Just (SchemeMainSchemeId m) <- [mainScheme]] \mainSchemeId ->
        placeThreatBy attrs W.ThreatFromMinion mainSchemeId (threat + minionBoost attrs)
    checkWindows [W.Window W.After $ W.SchemedByMinion (minionId attrs)]
    pure $ attrs {minionSchemeRemoval = Nothing} & boostL .~ 0
  MinionEndAttack -> pure $ attrs {minionActivationPlayer = Nothing,minionAttackTarget = Nothing,minionBoost = 0} & attackingL .~ Nothing
  MinionAttackGains k -> do
    lost <- elem (LoseKeyword k) <$> getModifiers attrs
    pure $ if lost then attrs else attrs & attackingL . _Just %~ gainKeyword k
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
        confirmEnemyAttack attack'
        pure $ attrs & boostL .~ 0 & attackingL ?~ attack'
  MinionDealtBoost card -> do
    checkWindows [W.Window W.After $ W.FacedownBoostGiven (EnemyMinionId $ minionId attrs) card]
    pure $ attrs & boostCardsL %~ (<> [card])
  -- each boost card is turned faceup in turn, and its icons added; one it is given as
  -- they are ("give this enemy another boost card") is turned up after them
  MinionFlipBoostCards -> do
    for_ (headMay $ minionBoostCards attrs) $ \card -> do
      you <- getYou
      chooseOnePromptedAbout you "$label.revealBoostCards" card
        [targeting card $ cardSpecific attrs "flipBoostCards" ()]
    pure attrs
  MinionAddBoost n -> pure $ attrs & boostL +~ n & attackingL . _Just . attackDamageL +~ n
 where
  isVillainous = elem Villainous <$> getModifiedKeywords attrs
  attackTarget ident = fromMaybe (IdentityCharacter ident) (minionAttackTarget attrs)
  attackWindow timing ident = W.Window timing $ case minionAttackTarget attrs of
    Just (AllyCharacter aid) -> W.EnemyAttackAlly (toEnemyId attrs) aid ident
    Just (VillainCharacter vid) -> W.EnemyAttackOnEnemy (toEnemyId attrs) (EnemyVillainId vid)
    Just (MinionCharacter mid) -> W.EnemyAttackOnEnemy (toEnemyId attrs) (EnemyMinionId mid)
    Just (AttachmentCharacter aid) -> W.EnemyAttackOnEnemy (toEnemyId attrs) (EnemyAttachmentId aid)
    _ -> W.EnemyAttack (toEnemyId attrs) ident
  newAttack ident = do
    base <- getModifiedAttackSigned attrs
    keywords <- getModifiedKeywords attrs
    prepareEnemyAttack (toEnemyId attrs) $ withAttackBase base $ attackWith keywords (toEnemyId attrs) (attackTarget ident) 0

instance RunMessage (Attrs Minion) where
  runMessage msg attrs = runQueueT do
    updated <- case msg of
      CardSpecific target "flipBoostCards" _ | isTarget attrs target -> flipMinionBoostCards attrs
      PlaceCounters (isTarget attrs -> True) n -> pure attrs {minionCounters=minionCounters attrs+n}
      RemoveCounters (isTarget attrs -> True) n -> pure attrs {minionCounters=subtractNatural n $ minionCounters attrs}
      CardSpecific target "schemeReplacesAttack" (fromJSON -> Success flag) | isTarget attrs target -> pure attrs {minionSchemeReplacesAttack = flag}
      CardSpecific target "fwReplaceBoost" (fromJSON -> Success (old, replacement)) | isTarget attrs target ->
        pure attrs {minionBoostCards = map (\card -> if card == old then replacement else card) (minionBoostCards attrs)}
      CardSpecific target "neRedirectAttackToAlly" (fromJSON -> Success (you,aid)) | target == toRef attrs ->
        pure attrs {minionActivationPlayer = Just you, minionAttackTarget = Just (AllyCharacter aid), minionAttacking = fmap (\a -> a {attackCharacter = AllyCharacter aid, attackTarget = Nothing}) (minionAttacking attrs)}
      RemovePlayerCardFromHeldZones cid -> pure attrs {minionHeldCards = filter ((/= cid) . pcCardId) (minionHeldCards attrs), minionHeldTreacheries = filter ((/= cid) . ecCardId) (minionHeldTreacheries attrs)}
      CardSpecific (isTarget attrs -> True) "ignoreWhenDefeated" (fromJSON -> Success flag) -> pure attrs {minionIgnoreWhenDefeated = flag}
      CardSpecific (isTarget attrs -> True) "informantScheme" (fromJSON -> Success you) -> pure attrs {minionSchemeRemoval = Just you}
      BeginPhase _ -> pure attrs {minionDamageThisPhase = 0}
      RanAbility _ (isTarget attrs -> True) 97001 _ _ -> do
        elite <- minionMatches (MinionWithTrait Elite) (minionId attrs)
        main <- selectOne EncounterMainScheme
        for_ main $ \scheme -> removeSchemeThreat attrs scheme (if elite then 2 else 1)
        pure attrs
      CardSpecific (isTarget attrs -> True) "holdTuckedCard" (fromJSON -> Success card) ->
        pure attrs {minionHeldCards=minionHeldCards attrs <> [card]}
      RanAbility _ (isTarget attrs -> True) 9901 _ _ -> do
        afterPendingMinionDefeat $ cardSpecific attrs "resolveTeamwork" ()
        pure attrs
      CardSpecific (isTarget attrs -> True) "resolveTeamwork" _ -> do
        let ident = minionEngagedIdentity attrs
        present <- selectAny (MinionWithId $ minionId attrs)
        keywords <- getModifiedKeywords attrs
        others <- anyM (\trait -> selectAny $ MinionWithTrait trait <> NotMinion (MinionWithId $ minionId attrs)) [trait | Teamwork trait <- keywords]
        hero <- identityMatches HeroIdentity ident
        when (present && others) $ enemyActivationAfterCurrent (minionId attrs) $
          if hero then enemyAttacks (minionId attrs) ident else minionSchemes (minionId attrs)
        pure attrs
      GiveStatus (MinionRef mid) s | mid == minionId attrs -> applyVulnerableStatus attrs s
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

applyVulnerableStatus :: ReverseQueue m => Attrs Minion -> StatusCard -> m (Attrs Minion)
applyVulnerableStatus attrs status = do
  replaced <- if status == ToughStatus then pure False else absorbStatusWithSolidSound (EnemyMinionId $ minionId attrs)
  updated <- if replaced then pure attrs else statusUnlessStalwart attrs status
  keywords <- getModifiedKeywords updated
  stunned <- isStunned updated
  confused <- isConfused updated
  when (Vulnerable `elem` keywords && (stunned || confused)) $ removeFromPlay updated
  pure updated
