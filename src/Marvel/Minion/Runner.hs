{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Minion.Runner (
  module Marvel.Minion.Runner,
  module X,
) where

import Marvel.Prelude

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
  stalwart <- elem Stalwart <$> getModifiedKeywords attrs
  pure $ if stalwart && s /= ToughStatus then attrs else attrs & statusL s .~ True

runMinionMessage ::
  (ReverseQueue m) => MinionMessage -> Attrs Minion -> m (Attrs Minion)
runMinionMessage msg attrs = case msg of
  MinionHealed n -> do
    pure $ attrs & damageL %~ subtractNatural n
  MinionHealAllDamage -> do
    pure $ attrs & damageL .~ 0
  MinionDamaged source damage -> do
    hitPoints <- getModifiedHitPoints attrs
    cannotTakeDamage <- elem CannotTakeDamage <$> getModifiers attrs
    if
      -- already defeated: damage to a minion on its way out of play does nothing
      | minionDamage attrs >= hitPoints -> pure attrs
      | cannotTakeDamage -> pure attrs
      | minionTough attrs && preventedByTough damage -> pure $ attrs & toughL .~ False
      | otherwise -> do
        when (damageAmount damage + minionDamage attrs >= hitPoints) do
          let excess = subtractNatural hitPoints (damageAmount damage + minionDamage attrs)
          -- "when your hero's attack deals excess damage, increase that amount"
          hero <- heroAttackBy damage source
          for_ hero \ident ->
            when (excess > 0) $ checkWindows [W.Window W.When $ W.ExcessDamageDealt ident (toEnemyId attrs) excess]
          minionStep (minionId attrs) (MinionDefeatedWithExcess source damage excess)
        when (damageAmount damage > 0) $
          checkWindows [W.Window W.After $ W.DamagedEnemy (toEnemyId attrs) source damage]
        pure $ attrs & damageL +~ damageAmount damage & toughL %~ (&& not (piercesTough damage))
  MinionDefeatedWithExcess source damage overkill -> do
    -- overkill follows the defeat: a minion that is not defeated after all deals none
    -- past it ('cancelMinionDefeat')
    checkWindows [W.Window W.When $ W.DefeatedMinion (minionId attrs) damage source overkill]
    defeatMinion (minionId attrs)
    when (overkill > 0 && damageOverkill damage) do
      traverse_ (\villain -> damageCharacter villain source (toDamage overkill FromOverkill)) =<< selectOne ActiveVillain
    checkWindows [W.Window W.After $ W.DefeatedMinion (minionId attrs) damage source overkill]
    pure attrs
  -- retaliate answers the attack whether or not a tough status card took its damage
  MinionRetaliates attacker -> do
    keywords <- getModifiedKeywords attrs
    for_ [n | Retaliate n <- toList keywords] (retaliate attrs attacker)
    pure attrs
  -- a stalwart minion cannot be stunned or confused (RR Stalwart)
  MinionStunned _ -> statusUnlessStalwart attrs StunnedStatus
  MinionConfused _ -> statusUnlessStalwart attrs ConfusedStatus
  MinionBecomeTough -> pure $ attrs & toughL .~ True
  MinionDefendedBy characterId ->
    pure $ attrs & attackingL . _Just %~ \a -> a {attackCharacter = characterId, attackDefended = True}
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
    when (isHero && Quickstrike `member` cdKeywords (getCardDef attrs)) $ enemyAttacks (minionId attrs) ident
    checkWindows [W.Window W.After $ W.EngagedMinion ident (minionId attrs)]
    pure attrs {minionEngagedIdentity = ident}
  -- with victory X, it and an attachment on it with victory X go to the victory display
  MinionDefeated -> do
    for_ (minionUpgrades attrs) removeFromPlay
    for_ (minionAttachments attrs) defeatedLeavesPlay
    defeatedLeavesPlay attrs
    minionDisengages (minionEngagedIdentity attrs) (minionId attrs)
    pure attrs
  -- a villainous minion is given a boost card as it uses a basic power (RR, Villainous)
  MinionSchemes ->
    if minionConfused attrs
      then pure $ attrs & confusedL .~ False
      else do
        villainous <- isVillainous
        when villainous do
          dealBoost attrs
          minionStep (minionId attrs) MinionFlipBoostCards
        minionSchemed (minionId attrs)
        when villainous clearBoosts
        pure attrs
  -- "attacks an ally": the player who controls it is attacked
  MinionAttacksAlly ident allyId -> do
    enemyAttacks (minionId attrs) ident
    pure attrs {minionAttackTarget = Just (AllyCharacter allyId)}
  MinionAttacks ident ->
    if minionStunned attrs
      then pure $ attrs & stunnedL .~ False & \a -> a {minionAttackTarget = Nothing}
      else do
        checkWindows [W.Window W.Would $ W.EnemyAttack (toEnemyId attrs) ident]
        minionBeginsAttack (minionId attrs) ident
        minionEndsAttack (minionId attrs)
        whenM isVillainous clearBoosts
        -- the attack is under way from when it is declared, as its would windows open
        attack' <- newAttack ident
        pure $ attrs & attackingL ?~ attack'
  MinionBeginAttack ident -> do
    attack' <- keepingGains (minionAttacking attrs) <$> newAttack ident
    villainous <- isVillainous
    checkWindows [W.Window W.When $ W.EnemyAttack (toEnemyId attrs) ident]
    when villainous $ dealBoost attrs
    declareDefense ident (toEnemyId attrs) (minionDefensePriority attrs)
    when villainous $ minionStep (minionId attrs) MinionFlipBoostCards
    minionAttacked (minionId attrs)
    checkWindows [W.Window W.After $ W.EnemyAttack (toEnemyId attrs) ident]
    pure $
      attrs {minionAttackTarget = Nothing} & attackingL ?~ attack'
  -- onto the main scheme of its area, if there is one there (as between The Once and
  -- Future Kang's stages)
  MinionSchemed -> do
    mainScheme <- selectOne MainScheme
    threat <- getModifiedScheme attrs
    for_ [m | Just (SchemeMainSchemeId m) <- [mainScheme]] \mainSchemeId ->
      placeThreatFrom W.ThreatFromMinion mainSchemeId (threat + minionBoost attrs)
    checkWindows [W.Window W.After $ W.SchemedByMinion (minionId attrs)]
    pure $ attrs & boostL .~ 0
  MinionEndAttack -> pure $ attrs & attackingL .~ Nothing
  MinionAttackGains k -> pure $ attrs & attackingL . _Just %~ gainKeyword k
  MinionAttackDamagePrevented -> pure $ attrs & attackingL . _Just %~ \a -> a {attackPrevented = True}
  MinionAttacked -> do
    case minionAttacking attrs of
      Nothing -> error "No current attack"
      Just attack' -> do
        attackedYou (attackEnemy attack') (attackCharacter attack')
        wasAttacked attack'
    pure $ attrs & boostL .~ 0
  MinionDealtBoost card -> pure $ attrs & boostCardsL %~ (<> [card])
  -- each boost card is turned faceup in turn, and its icons added; one it is given as
  -- they are ("give this enemy another boost card") is turned up after them
  MinionFlipBoostCards -> do
    for_ (minionBoostCards attrs) \card -> do
      checkWindows [W.Window W.When $ W.FlippedBoostCard card (toEnemyId attrs)]
      revealBoostCard card (minionId attrs)
      countActivationBoostIcons attrs [card]
    unless (null $ minionBoostCards attrs) $ minionStep (minionId attrs) MinionFlipBoostCards
    pure $ attrs & boostCardsL .~ []
  MinionAddBoost n -> pure $ attrs & boostL +~ n & attackingL . _Just . attackDamageL +~ n
 where
  isVillainous = elem Villainous <$> getModifiedKeywords attrs
  attackTarget ident = fromMaybe (IdentityCharacter ident) (minionAttackTarget attrs)
  newAttack ident = attackWith <$> getModifiedKeywords attrs <*> pure (toEnemyId attrs) <*> pure (attackTarget ident) <*> getModifiedAttack attrs

instance RunMessage (Attrs Minion) where
  runMessage msg attrs = runQueueT $ case msg of
    GiveStatus (MinionRef mid) s | mid == minionId attrs -> statusUnlessStalwart attrs s
    RemoveStatus (MinionRef mid) s | mid == minionId attrs -> pure $ attrs & statusL s .~ False
    MinionMessage ident msg'
      | ident == minionId attrs ->
          runMinionMessage msg' attrs
    AttachmentRemoved attachmentId ->
      pure $ attrs & attachmentsL %~ HashSet.delete attachmentId
    WithBoostIcons (MinionRef mid) _ counts | mid == minionId attrs -> do
      minionStep mid (MinionAddBoost $ sum $ map snd counts)
      pure attrs
    _ -> pure attrs
