module Marvel.Upgrade.Upgrades.PhasedAndConfused (
  phasedAndConfused,
  PhasedAndConfused (..),
) where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status ()

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

phasedAndConfused :: UpgradeCard PhasedAndConfused
phasedAndConfused = upgrade PhasedAndConfused Cards.phasedAndConfused

newtype PhasedAndConfused = PhasedAndConfused (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PhasedAndConfused where
  getAbilities (PhasedAndConfused attrs) = case upgradeAttachedEnemy attrs of
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

instance IsUpgrade PhasedAndConfused where
  upgradeTargets _ = Just . map toTarget <$> selectList (NotEnemy $ EnemyWithUpgrade $ UpgradeIs Cards.phasedAndConfused)

instance RunMessage PhasedAndConfused where
  runMessage msg u@(PhasedAndConfused a) = runQueueT $ case msg of
    UpgradeMessage ident msg' | ident == upgradeId a -> case msg' of
      _ -> PhasedAndConfused <$> runMessage msg a
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      case upgradeAttachedEnemy a of
        Just enemyId -> do
          flip insteadOf (removeFromPlay a >> confuse enemyId a) \case
            VillainMessage vid (VillainBeginAttack _) -> enemyId == EnemyVillainId vid
            VillainMessage vid (VillainBeginAttackWithoutBoost _) -> enemyId == EnemyVillainId vid
            MinionMessage mid (MinionBeginAttack _) -> enemyId == EnemyMinionId mid
            _ -> False
          cancelMatchingMessage $ \case
            CheckWindows [Window Would (EnemyAttack enemyId' _)] ->
              enemyId == enemyId'
            _ -> False
        Nothing -> error "Something terrible must have happened"
      pure u
    _ -> PhasedAndConfused <$> runMessage msg a
