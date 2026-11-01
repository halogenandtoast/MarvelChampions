module Marvel.Upgrade.Upgrades.SuppressingFire where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Upgrade.Types

suppressingFire :: UpgradeCard SuppressingFire
suppressingFire = upgrade SuppressingFire Cards.suppressingFire

newtype SuppressingFire = SuppressingFire (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade SuppressingFire where
  upgradeTargets _ = Just . map enemyRef <$> selectList (MinionEnemy <> NotEnemy (EnemyWithUpgrade $ UpgradeIs Cards.suppressingFire))
instance HasAbilities SuppressingFire where
  getAbilities a@(SuppressingFire attrs) = case upgradeAttachedEnemy attrs of
    Just enemy -> [limitedWindowAbility a 1 (W.EnemyDefeated W.When (EnemyWithId enemy) $ AttackFromPlayer $ IdentityWithId $ upgradeController attrs)
      HeroInterrupt OwnsThis NoCost (runAbility a 1)]
    Nothing -> []
instance RunMessage SuppressingFire where
  runMessage msg a@(SuppressingFire attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ heal you 2
    _ -> SuppressingFire <$> runMessage msg attrs
