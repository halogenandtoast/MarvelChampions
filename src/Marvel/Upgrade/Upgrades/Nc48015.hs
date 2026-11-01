module Marvel.Upgrade.Upgrades.Nc48015 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Upgrade.Types
import Marvel.Upgrade.Cards qualified as Cards

nc48015 :: UpgradeCard Nc48015
nc48015 = upgrade Nc48015 Cards.nc48015

newtype Nc48015 = Nc48015 (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade Nc48015 where
  upgradeTargets _ = Just . map toTarget <$> selectList AnyMinion

instance HasAbilities Nc48015 where
  getAbilities a@(Nc48015 attrs) = case upgradeAttachedEnemy attrs of
    Just enemy -> [limitedWindowAbility a 1 (W.HeroDefendedWithoutDamageAgainst W.After AnyIdentity $ EnemyWithId enemy)
      Response NoCriteria NoCost (runAbility a 1)]
    _ -> []

instance RunMessage Nc48015 where
  runMessage msg a@(Nc48015 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (upgradeAttachedEnemy attrs) $ \enemy -> dealDamage enemy attrs (toDamage 4 FromAbility)
      pure a
    _ -> Nc48015 <$> runMessage msg attrs
