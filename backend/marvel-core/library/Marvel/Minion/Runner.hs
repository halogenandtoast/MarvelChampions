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
import Marvel.Message as X
import Marvel.Minion.Types as X hiding (Field (..))
import Marvel.Modifier as X
import Marvel.Query as X
import Marvel.Question as X
import Marvel.Queue as X
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

runMinionMessage ::
  (HasQueue m, HasGame m) => MinionMessage -> Attrs Minion -> m (Attrs Minion)
runMinionMessage msg attrs = case msg of
  MinionHealed n -> do
    pure $ attrs & damageL %~ subtractNatural n
  MinionHealAllDamage -> do
    pure $ attrs & damageL .~ 0
  MinionDamaged source damage ->
    if minionTough attrs
      then pure $ attrs & toughL .~ False
      else do
        keywords <- getModifiedKeywords attrs
        hitPoints <- getModifiedHitPoints attrs
        if (damageAmount damage + minionDamage attrs >= hitPoints)
          then do
            let overkill = damageAmount damage - (hitPoints - minionDamage attrs)
            when (overkill > 0 && damageOverkill damage) $ do
              villain <- selectJust ActiveVillain
              push $
                VillainMessage
                  villain
                  (VillainDamaged source (toDamage overkill FromOverkill))

            pushAll
              [ CheckWindows
                  [W.Window W.When $ W.DefeatedMinion (minionId attrs) damage]
              , MinionMessage (minionId attrs) MinionDefeated
              , CheckWindows
                  [W.Window W.After $ W.DefeatedMinion (minionId attrs) damage]
              ]
          else for_ keywords $ \case
            Retaliate n -> case damageSource damage of
              FromPlayerAttack ident ->
                push $
                  IdentityMessage
                    ident
                    (IdentityDamaged (toSource attrs) (toDamage n FromRetaliate))
              FromAllyAttack ident ->
                push $
                  AllyMessage
                    ident
                    (AllyDamaged (toSource attrs) (toDamage n FromRetaliate))
              _ -> pure ()
            _ -> pure ()
        pure $ attrs & damageL +~ damageAmount damage
  MinionStunned _ -> pure $ attrs & stunnedL .~ True
  MinionConfused _ -> pure $ attrs & confusedL .~ True
  MinionBecomeTough -> pure $ attrs & toughL .~ True
  MinionDefendedBy characterId ->
    pure $ attrs & attackingL . _Just . attackCharacterL .~ characterId
  AttachedToMinion attachmentId -> do
    pure $ attrs & attachmentsL %~ HashSet.insert attachmentId
  AttachedUpgradeToMinion upgradeId ->
    pure $ attrs & upgradesL %~ HashSet.insert upgradeId
  RevealMinion _ -> pure attrs
  MinionEngagedIdentity ident -> do
    isHero <- identityMatches HeroIdentity ident
    when
      (isHero && Quickstrike `member` cdKeywords (getCardDef attrs))
      (push $ MinionMessage (minionId attrs) $ MinionAttacks ident)
    pure attrs
  MinionDefeated -> do
    pushAll $
      map (RemoveFromPlay . toRef) (toList $ minionUpgrades attrs)
        <> [ RemoveFromPlay (toRef attrs)
           , IdentityMessage
              (minionEngagedIdentity attrs)
              (MinionDisengaged $ minionId attrs)
           ]
    pure attrs
  MinionSchemes ->
    if minionConfused attrs
      then pure $ attrs & confusedL .~ False
      else do
        push $ MinionMessage (minionId attrs) MinionSchemed
        pure attrs
  MinionAttacks ident ->
    if minionStunned attrs
      then pure $ attrs & stunnedL .~ False
      else do
        pushAll
          [ CheckWindows
              [W.Window W.Would $ W.EnemyAttack (toEnemyId attrs) ident]
          , MinionMessage (minionId attrs) (MinionBeginAttack ident)
          ]
        pure attrs
  MinionBeginAttack ident -> do
    atk <- getModifiedAttack attrs
    pushAll
      [ CheckWindows [W.Window W.When $ W.EnemyAttack (toEnemyId attrs) ident]
      , DeclareDefense ident (toEnemyId attrs) (minionDefensePriority attrs)
      , MinionMessage (minionId attrs) MinionAttacked
      , CheckWindows [W.Window W.After $ W.EnemyAttack (toEnemyId attrs) ident]
      ]
    pure $
      attrs
        & attackingL
          ?~ attack (toEnemyId attrs) (IdentityCharacter ident) atk
  MinionSchemed -> do
    mainScheme <- selectJust MainScheme
    case mainScheme of
      SchemeMainSchemeId mainSchemeId -> do
        let threat = unSch (minionScheme attrs)
        pushAll
          [ CheckWindows
              [ W.Window W.Would $
                  W.ThreatPlaced
                    W.ThreatFromMinion
                    (SchemeMainSchemeId mainSchemeId)
                    threat
              ]
          , MainSchemeMessage mainSchemeId $ MainSchemePlaceThreat threat
          ]
        pure attrs
      _ -> error "Not the main scheme"
  MinionAttacked -> do
    case minionAttacking attrs of
      Nothing -> error "No current attack"
      Just attack' -> case attackCharacter attack' of
        IdentityCharacter ident ->
          push $ IdentityMessage ident $ IdentityWasAttacked attack'
        AllyCharacter ident ->
          push $ AllyMessage ident $ AllyWasAttacked attack'
        _ -> error "Invalid damage target"
    pure attrs

instance RunMessage (Attrs Minion) where
  runMessage msg attrs = case msg of
    MinionMessage ident msg'
      | ident == minionId attrs ->
          runMinionMessage msg' attrs
    _ -> pure attrs
