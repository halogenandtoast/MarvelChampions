{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Ally.Runner
  ( module X
  ) where

import Marvel.Prelude

import Marvel.Ally.Types as X hiding (Field(..))
import Marvel.Message.Lifted as X
import Marvel.Question as X

import Data.HashSet qualified as HashSet
import Marvel.Ability
import Marvel.Attack
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Query
import Marvel.Window qualified as W

instance HasAbilities Ally where
  getAbilities x@(Ally a) = getAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability
          x
          300
          Basic
          -- "you cannot thwart" (Baron Zemo) keeps the identity from thwarting, not
          -- its allies
          (SchemeExists ThwartableScheme)
          ExhaustCost
          (AllyThwart $ toId x)
      | unThw (allyThwart $ toAllyAttrs a) > 0
      ]
      <> [ ability
             x
             301
             Basic
             -- a stunned ally can attempt an attack with no target ("Stunned")
             (if allyStunned (toAllyAttrs a) then NoCriteria else EnemyExists $ AttackableBy $ IdentityWithId $ allyController $ toAllyAttrs a)
             (ExhaustCost <> allyAdditionalAttackCost (toAllyAttrs a))
             (AllyAttack $ toId x)
         ]

statusL :: StatusCard -> Lens' (Attrs Ally) Bool
statusL = \case
  StunnedStatus -> stunnedL
  ConfusedStatus -> confusedL
  ToughStatus -> toughL

instance RunMessage (Attrs Ally) where
  runMessage msg a = runQueueT $ case msg of
    AllyMessage ident msg' | ident == allyId a -> case msg' of
      ReadiedAlly -> do
        pure $ a & exhaustedL .~ False
      ExhaustedAlly -> do
        pure $ a & exhaustedL .~ True
      AllyStunned -> do
        pure $ a & stunnedL .~ True
      AllyAttacked -> if allyStunned a
        then pure $ a & stunnedL .~ False
        else do
          enemies <- selectList $ AttackableBy $ IdentityWithId $ allyController a
          dmg <- toDamage <$> getModifiedAttack a <*> pure (FromAllyAttack $ allyId a)
          chooseOne (allyController a)
            [ targeting (enemyRef enemy) do
                checkWindows [W.Window W.When $ W.AllyAttack (allyId a) enemy]
                allyAttackLands (allyId a) enemy dmg
            | enemy <- enemies
            ]
          pure a
      -- an interrupt that defeats the enemy aborts the attack: no damage and no
      -- consequential damage (RR, Consequential Damage)
      AllyAttackLands enemy dmg -> do
        stillInPlay <- selectAny (EnemyWithId enemy)
        consequential <- getModifiedAttackConsequentialDamage a
        when stillInPlay do
          dealDamage enemy a dmg
          enemyRetaliates enemy (AllyCharacter $ allyId a)
          checkWindows [W.Window W.After $ W.AllyAttack (allyId a) enemy]
          when (consequential > 0) $ damageCharacter ident a (toDamage consequential FromConsequential)
        pure a
      AllyThwarted -> if allyConfused a
        then pure $ a & confusedL .~ False
        else do
          schemes <- selectList ThwartableScheme
          thw <- getModifiedThwart a
          chooseOne (allyController a)
            [ targeting (schemeRef scheme) do
                thwartScheme scheme a thw
                checkWindows [W.Window W.After $ W.AllyThwart (allyId a) scheme]
            | scheme <- schemes
            ]
          when (allyThwartConsequentialDamage a > 0) $
            damageCharacter ident a (toDamage (allyThwartConsequentialDamage a) FromConsequential)
          pure a
      AllyDefended enemyId -> do
        exhaust ident
        defendedBy enemyId ident
        pure a
      AllyHealed n -> pure $ a & damageL %~ subtractNatural n
      AllyWasAttacked attack' -> do
        hitPoints <- getModifiedHitPoints a
        let
          overkill = subtractNatural
            (subtractNatural (allyDamage a) hitPoints)
            (attackDamage attack')
        damageCharacter ident (attackSource attack') (toDamage (attackDamage attack') FromAttack)
        when (attackOverkill attack' && overkill > 0) $
          attackDamageIdentity (allyController a) (attackSource attack') (toDamage overkill FromAttack)
        pure a
      AllyDamaged source damage -> if allyTough a && not (damageIgnoresTough damage)
        then pure $ a & toughL .~ False
        else do
          hitPoints <- getModifiedHitPoints a
          attackDealtDamageTo source damage ident
          when (damageAmount damage + allyDamage a >= hitPoints) $ defeatAlly ident
          pure $ a & damageL +~ damageAmount damage
      -- leaving play discards it
      AllyDefeated -> do
        let card = MkPlayerCard (CardId $ unAllyId $ allyId a) (allyCardDef a) (Just $ allyController a) (Just $ allyController a)
        checkWindows [W.Window W.When $ W.DefeatedAlly (allyId a)]
        -- which discards it
        removeFromPlay a
        checkWindows [W.Window W.After $ W.AllyCardDefeated (allyController a) card]
        pure a
      SpendAllyUse -> pure $ a & countersL -~ 1
      AllyPaidWith payment -> pure a {allyPaidWith = payment}
      AttachedUpgradeToAlly upgradeId ->
        pure $ a & upgradesL %~ HashSet.insert upgradeId
    -- "if an ability that says an ally gets +X hit points ceases to be in effect and
    -- causes that ally to have damage on it equal to or greater than its hit points,
    -- that ally is defeated"
    UpgradeRemoved upgradeId | HashSet.member upgradeId (allyUpgrades a) -> do
      hitPoints <- getModifiedHitPoints a
      when (allyDamage a > 0 && allyDamage a >= hitPoints) $ defeatAlly (allyId a)
      pure $ a & upgradesL %~ HashSet.delete upgradeId
    GiveStatus (AllyRef ident) s | ident == allyId a -> pure $ a & statusL s .~ True
    RemoveStatus (AllyRef ident) s | ident == allyId a -> pure $ a & statusL s .~ False
    _ -> pure a
