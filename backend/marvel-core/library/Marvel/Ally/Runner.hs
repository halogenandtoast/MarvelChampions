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
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Query

instance HasAbilities Ally where
  getAbilities x@(Ally a) = getAbilities a <> basicAbilities
   where
    basicAbilities =
      [ ability
          x
          300
          Basic
          (SchemeExists ThwartableScheme)
          ExhaustCost
          (AllyThwart $ toId x)
      | unThw (allyThwart $ toAllyAttrs a) > 0
      ]
      <> [ ability
             x
             301
             Basic
             (EnemyExists AttackableEnemy)
             ExhaustCost
             (AllyAttack $ toId x)
         ]

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
          pushAll
            $ Ask
                (allyController a)
                (ChooseOne $ map
                  (damageChoice a (toDamage dmg $ FromAllyAttack (allyId a)))
                  enemies
                )
            : [ AllyMessage
                  ident
                  (AllyDamaged
                    (toSource a)
                    (toDamage
                      (allyAttackConsequentialDamage a)
                      FromConsequential
                    )
                  )
              | allyAttackConsequentialDamage a > 0
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
        let
          overkill = subtractNatural
            (unHp (allyHitPoints a) - allyDamage a)
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
      AllyDamaged _ damage -> if allyTough a
        then pure $ a & toughL .~ False
        else do
          when
            (damageAmount damage + allyDamage a >= unHp (allyHitPoints a))
            (push $ AllyMessage (allyId a) AllyDefeated)
          pure $ a & damageL +~ damageAmount damage
      AllyDefeated -> do
        pushAll [RemoveFromPlay (toTarget a), DiscardedCard (toCard a)]
        pure a
      SpendAllyUse -> pure $ a & countersL -~ 1
      AttachedUpgradeToAlly upgradeId ->
        pure $ a & upgradesL %~ HashSet.insert upgradeId
    _ -> pure a
