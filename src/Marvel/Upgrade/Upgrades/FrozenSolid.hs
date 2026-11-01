module Marvel.Upgrade.Upgrades.FrozenSolid where

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
import Marvel.Iceman.Helpers
import Marvel.Upgrade.Types

frozenSolid :: UpgradeCard FrozenSolid
frozenSolid = upgrade FrozenSolid Cards.frozenSolid

newtype FrozenSolid = FrozenSolid (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade FrozenSolid where
  upgradeTargets _ = Just . map enemyRef <$> selectList (NotEnemy $ EnemyWithUpgrade $ UpgradeIs Cards.frozenSolid)
instance HasAbilities FrozenSolid where
  getAbilities a@(FrozenSolid attrs) = case upgradeAttachedEnemy attrs of
    Just enemy -> [limitedWindowAbility a 1 (W.EnemyActivates W.When (EnemyWithId enemy) AnyIdentity) ForcedInterrupt OwnsThis NoCost (runAbility a 1)]
    Nothing -> []
instance RunMessage FrozenSolid where
  runMessage msg a@(FrozenSolid attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      for_ (upgradeAttachedEnemy attrs) $ \enemy -> do
        cancelEnemyActivation enemy
        removeFromPlay attrs
        attachFrostbite you enemy
      pure a
    _ -> FrozenSolid <$> runMessage msg attrs
