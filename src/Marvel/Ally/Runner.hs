{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Ally.Runner
  ( module X
  ) where

import Marvel.Prelude

import Marvel.Ally.Types as X hiding (Field(..))
import Marvel.Message as X
import Marvel.Question as X

import Data.HashSet qualified as HashSet
import Marvel.Ability
import Marvel.Attack
import Marvel.Choice
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
          (SchemeExists ThwartableScheme <> SelfMatches IdentityCanThwart)
          ExhaustCost
          (AllyThwart $ toId x)
      | unThw (allyThwart $ toAllyAttrs a) > 0
      ]
      <> [ ability
             x
             301
             Basic
             (EnemyExists AttackableEnemy)
             (ExhaustCost <> allyAdditionalAttackCost (toAllyAttrs a))
             (AllyAttack $ toId x)
         ]

statusL :: StatusCard -> Lens' (Attrs Ally) Bool
statusL = \case
  StunnedStatus -> stunnedL
  ConfusedStatus -> confusedL
  ToughStatus -> toughL

instance RunMessage (Attrs Ally) where
  runMessage msg a = case msg of
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
          enemies <- selectList AttackableEnemy
          dmg <- getModifiedAttack a
          push $
            Ask
              (allyController a)
              (ChooseOne $ map (damageChoice a (toDamage dmg $ FromAllyAttack (allyId a))) enemies)
          pure a
      -- an interrupt that defeats the enemy aborts the attack: no damage and no
      -- consequential damage (RR, Consequential Damage)
      AllyAttackLands enemy dmg -> do
        stillInPlay <- selectAny (EnemyWithId enemy)
        consequential <- getModifiedAttackConsequentialDamage a
        damage <- choiceMessages (allyController a) (DamageEnemy (toRef enemy) (toRef a) dmg)
        when stillInPlay $
          pushAll $
            damage
              <> [CheckWindows [W.Window W.After $ W.AllyAttack (allyId a) enemy]]
              <> [ AllyMessage ident (AllyDamaged (toSource a) (toDamage consequential FromConsequential))
                 | consequential > 0
                 ]
        pure a
      AllyThwarted -> if allyConfused a
        then pure $ a & confusedL .~ False
        else do
          schemes <- selectList ThwartableScheme
          thw <- getModifiedThwart a
          pushAll
            $ Ask
                (allyController a)
                (ChooseOne $ map (thwartChoice a thw) schemes)
            : [ AllyMessage
                  ident
                  (AllyDamaged
                    (toSource a)
                    (toDamage
                      (allyThwartConsequentialDamage a)
                      FromConsequential
                    )
                  )
              | allyThwartConsequentialDamage a > 0
              ]
          pure a
      AllyDefended enemyId -> do
        pushAll
          [ AllyMessage (allyId a) ExhaustedAlly
          , case enemyId of
            EnemyVillainId vid ->
              VillainMessage vid $ VillainDefendedBy (AllyCharacter $ allyId a)
            EnemyMinionId vid ->
              MinionMessage vid $ MinionDefendedBy (AllyCharacter $ allyId a)
          ]
        pure a
      AllyHealed n -> pure $ a & damageL %~ subtractNatural n
      AllyWasAttacked attack' -> do
        hitPoints <- getModifiedHitPoints a
        let
          overkill = subtractNatural
            (subtractNatural (allyDamage a) hitPoints)
            (attackDamage attack')
        when
          (attackOverkill attack' && overkill > 0)
          (push $ IdentityMessage (allyController a) $ IdentityDamaged
            (attackSource attack')
            (toDamage overkill FromAttack)
          )

        push $ AllyMessage ident $ AllyDamaged
          (attackSource attack')
          (toDamage (attackDamage attack') FromAttack)
        pure a
      AllyDamaged source damage -> if allyTough a && not (damageIgnoresTough damage)
        then pure $ a & toughL .~ False
        else do
          hitPoints <- getModifiedHitPoints a
          pushAll $
            attackDealtDamage source damage (AllyCharacter $ allyId a)
              <> [AllyMessage (allyId a) AllyDefeated | damageAmount damage + allyDamage a >= hitPoints]
          pure $ a & damageL +~ damageAmount damage
      -- leaving play discards it
      AllyDefeated -> do
        let card = MkPlayerCard (CardId $ unAllyId $ allyId a) (allyCardDef a) (Just $ allyController a) (Just $ allyController a)
        pushAll
          [ CheckWindows [W.Window W.When $ W.DefeatedAlly (allyId a)]
          , -- which discards it
            RemoveFromPlay (toTarget a)
          , CheckWindows [W.Window W.After $ W.AllyCardDefeated (allyController a) card]
          ]
        pure a
      SpendAllyUse -> pure $ a & countersL -~ 1
      AllyPaidWith payment -> pure a {allyPaidWith = payment}
      AttachedUpgradeToAlly upgradeId ->
        pure $ a & upgradesL %~ HashSet.insert upgradeId
    GiveStatus (AllyRef ident) s | ident == allyId a -> pure $ a & statusL s .~ True
    RemoveStatus (AllyRef ident) s | ident == allyId a -> pure $ a & statusL s .~ False
    _ -> pure a
