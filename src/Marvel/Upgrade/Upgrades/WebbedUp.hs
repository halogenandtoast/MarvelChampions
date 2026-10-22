module Marvel.Upgrade.Upgrades.WebbedUp (
  webbedUp,
  WebbedUp (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

webbedUp :: UpgradeCard WebbedUp
webbedUp = upgrade WebbedUp Cards.webbedUp

newtype WebbedUp = WebbedUp (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WebbedUp where
  getAbilities (WebbedUp attrs) = case upgradeAttachedEnemy attrs of
    Just enemyId ->
      [ windowAbility
          attrs
          1
          (EnemyWouldAttack (EnemyWithId enemyId) AnyIdentity)
          ForcedInterrupt
          NoCost
          $ TargetLabel (toRef attrs) [RunAbility (toRef attrs) 1]
      ]
    _ -> []

instance IsUpgrade WebbedUp where
  upgradeTargets _ = Just . map toTarget <$> selectList (NotEnemy $ EnemyWithUpgrade $ UpgradeIs Cards.webbedUp)

instance RunMessage WebbedUp where
  runMessage msg u@(WebbedUp a) = runQueueT $ case msg of
    UpgradeMessage ident msg' | ident == upgradeId a -> case msg' of
      _ -> WebbedUp <$> runMessage msg a
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      case upgradeAttachedEnemy a of
        Just enemyId -> do
          flip insteadOf (removeFromPlay a >> stun enemyId a) \case
            VillainMessage vid (VillainBeginAttack _) -> enemyId == EnemyVillainId vid
            MinionMessage mid (MinionBeginAttack _) -> enemyId == EnemyMinionId mid
            _ -> False
          cancelMatchingMessage $ \case
            CheckWindows [Window Would (EnemyAttack enemyId' _)] ->
              enemyId == enemyId'
            _ -> False
        Nothing -> error "Something terrible must have happened"
      pure u
    _ -> WebbedUp <$> runMessage msg a
