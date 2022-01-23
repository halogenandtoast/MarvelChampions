module Marvel.Upgrade.Upgrades.WebbedUp
  ( webbedUp
  , WebbedUp(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Window

webbedUp :: UpgradeCard WebbedUp
webbedUp = upgrade WebbedUp Cards.webbedUp

newtype WebbedUp = WebbedUp UpgradeAttrs
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities WebbedUp where
  getAbilities (WebbedUp attrs) = case upgradeAttachedEnemy attrs of
    Just enemyId ->
      [ windowAbility
            attrs
            1
            (EnemyWouldAttack (EnemyWithId enemyId) AnyIdentity)
            ForcedInterrupt
            NoCost
          $ TargetLabel (toTarget attrs) [RunAbility (toTarget attrs) 1]
      ]
    _ -> []

instance RunMessage WebbedUp where
  runMessage msg u@(WebbedUp a) = case msg of
    UpgradeMessage upgradeId msg' | upgradeId == toId a -> case msg' of
      PlayedUpgrade -> do
        enemies <- selectList AnyEnemy
        chooseOne (upgradeController a) $ map
          (\enemyId -> TargetLabel
            (EnemyTarget enemyId)
            [Run [UpgradeMessage (toId a) $ UpgradeAttachedToEnemy enemyId]]
          )
          enemies
        pure u
      _ -> WebbedUp <$> runMessage msg a
    RanAbility target 1 _ | isTarget a target -> do
      case upgradeAttachedEnemy a of
        Just enemyId -> do
          replaceMatchingMessage
              (const [ RemoveFromPlay (toTarget a)
              , case enemyId of
                EnemyVillainId villainId ->
                  VillainMessage villainId (VillainStunned $ toSource a)
                EnemyMinionId minionId ->
                  MinionMessage minionId (MinionStunned $ toSource a)
              ])
            $ \case
                VillainMessage _ (VillainBeginAttack _) -> True
                _ -> False
          cancelMatchingMessage $ \case
            CheckWindows [Window Would (EnemyAttack enemyId' _)] ->
              enemyId == enemyId'
            _ -> False
        Nothing -> error "Something terrible must have happened"
      pure u
    _ -> WebbedUp <$> runMessage msg a
