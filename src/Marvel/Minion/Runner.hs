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
      | minionTough attrs && not (damageIgnoresTough damage) -> pure $ attrs & toughL .~ False
      | otherwise -> do
        if (damageAmount damage + minionDamage attrs >= hitPoints)
          then do
            -- overkill follows the defeat: a minion that is not defeated after all
            -- deals none past it ('cancelMinionDefeat')
            let overkill = subtractNatural hitPoints (damageAmount damage + minionDamage attrs)
            checkWindows [W.Window W.When $ W.DefeatedMinion (minionId attrs) damage source]
            defeatMinion (minionId attrs)
            when (overkill > 0 && damageOverkill damage) do
              villain <- selectJust ActiveVillain
              damageCharacter villain source (toDamage overkill FromOverkill)
            checkWindows [W.Window W.After $ W.DefeatedMinion (minionId attrs) damage source]
          else pure ()
        pure $ attrs & damageL +~ damageAmount damage
  -- retaliate answers the attack whether or not a tough status card took its damage
  MinionRetaliates attacker -> do
    keywords <- getModifiedKeywords attrs
    for_ [n | Retaliate n <- toList keywords] \n -> damageCharacter attacker attrs (toDamage n FromRetaliate)
    pure attrs
  MinionStunned _ -> pure $ attrs & stunnedL .~ True
  MinionConfused _ -> pure $ attrs & confusedL .~ True
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
  MinionDefeated -> do
    for_ (minionUpgrades attrs) removeFromPlay
    for_ (minionAttachments attrs) removeFromPlay
    removeFromPlay attrs
    minionDisengages (minionEngagedIdentity attrs) (minionId attrs)
    pure attrs
  MinionSchemes ->
    if minionConfused attrs
      then pure $ attrs & confusedL .~ False
      else do
        minionSchemed (minionId attrs)
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
        -- the attack is under way from when it is declared, as its would windows open
        atk <- getModifiedAttack attrs
        pure $ attrs & attackingL ?~ attack (toEnemyId attrs) (attackTarget ident) atk
  MinionBeginAttack ident -> do
    atk <- getModifiedAttack attrs
    checkWindows [W.Window W.When $ W.EnemyAttack (toEnemyId attrs) ident]
    declareDefense ident (toEnemyId attrs) (minionDefensePriority attrs)
    minionAttacked (minionId attrs)
    checkWindows [W.Window W.After $ W.EnemyAttack (toEnemyId attrs) ident]
    pure $
      attrs {minionAttackTarget = Nothing}
        & attackingL
          ?~ attack (toEnemyId attrs) (attackTarget ident) atk
  MinionSchemed -> do
    mainScheme <- selectJust MainScheme
    case mainScheme of
      SchemeMainSchemeId mainSchemeId -> do
        threat <- getModifiedScheme attrs
        when (threat > 0) $
          checkWindows [W.Window W.Would $ W.ThreatPlaced W.ThreatFromMinion mainScheme threat]
        threatPlaced mainSchemeId threat
        checkWindows [W.Window W.After $ W.SchemedByMinion (minionId attrs)]
        pure attrs
      _ -> error "Not the main scheme"
  MinionEndAttack -> pure $ attrs & attackingL .~ Nothing
  MinionAttacked -> do
    case minionAttacking attrs of
      Nothing -> error "No current attack"
      Just attack' -> do
        attackedYou (attackEnemy attack') (attackCharacter attack')
        characterAttacked attack'
    pure attrs
 where
  attackTarget ident = fromMaybe (IdentityCharacter ident) (minionAttackTarget attrs)

instance RunMessage (Attrs Minion) where
  runMessage msg attrs = runQueueT $ case msg of
    GiveStatus (MinionRef mid) s | mid == minionId attrs -> pure $ attrs & statusL s .~ True
    RemoveStatus (MinionRef mid) s | mid == minionId attrs -> pure $ attrs & statusL s .~ False
    MinionMessage ident msg'
      | ident == minionId attrs ->
          runMinionMessage msg' attrs
    AttachmentRemoved attachmentId ->
      pure $ attrs & attachmentsL %~ HashSet.delete attachmentId
    _ -> pure attrs
