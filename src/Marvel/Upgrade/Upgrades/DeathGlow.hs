module Marvel.Upgrade.Upgrades.DeathGlow (deathGlow, DeathGlow (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

deathGlow :: UpgradeCard DeathGlow
deathGlow = upgrade DeathGlow Cards.deathGlow

newtype DeathGlow = DeathGlow (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade DeathGlow where
  upgradeTargets _ = Just . map enemyRef <$> selectList AnyEnemy

instance HasAbilities DeathGlow where
  getAbilities a@(DeathGlow attrs) =
    [ limitedWindowAbility a 1 (W.EnemyDefeatedWithUpgrade W.When Cards.deathGlow AnyDamage)
        ForcedInterrupt OwnsThis NoCost (runAbility a 1)
    | isJust (upgradeAttachedEnemy attrs)
    ]

instance RunMessage DeathGlow where
  runMessage msg u@(DeathGlow attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [ (damage, source)
           | W.DefeatedEnemyWithUpgrades enemy _ damage source <- windows
           , Just enemy == upgradeAttachedEnemy attrs
           ] $ \(damage, source) -> do
        defeatedByValkyrie <- damageMatchesFrom (DamageByPlayer $ IdentityWithId $ upgradeController attrs) damage source
        setAside [toCard attrs]
        when defeatedByValkyrie $ ready (upgradeController attrs)
      pure u
    _ -> DeathGlow <$> runMessage msg attrs
