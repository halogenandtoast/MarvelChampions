module Marvel.Upgrade.Upgrades.WebbedUp
  ( webbedUp
  , WebbedUp(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Window

webbedUp :: UpgradeCard WebbedUp
webbedUp = upgrade WebbedUp Cards.webbedUp

newtype WebbedUp = WebbedUp UpgradeAttrs
  deriving anyclass IsUpgrade
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
          $ TargetLabel
              (toTarget attrs)
              [ DiscardTarget (toTarget attrs)
              , Stun (EnemyTarget enemyId) (toSource attrs)
              ]
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
            [Run [UpgradeMessage (toId a) $ AttachedToEnemy enemyId]]
          )
          enemies
        pure u
      _ -> WebbedUp <$> runMessage msg a
    _ -> WebbedUp <$> runMessage msg a
