module Marvel.Upgrade.Upgrades.Nc48006 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
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
import Marvel.Id
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

nc48006 :: UpgradeCard Nc48006
nc48006 = upgrade Nc48006 Cards.nc48006

newtype Nc48006 = Nc48006 (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade Nc48006 where
  upgradeTargets _ = Just . map enemyRef <$> selectList AnyEnemy

instance HasAbilities Nc48006 where
  getAbilities a@(Nc48006 attrs) = case upgradeAttachedEnemy attrs of
    Nothing -> []
    Just enemy -> [subtype Defense $ limitedWindowAbility a 1
      (W.EnemyAttackedAnyTarget W.When $ EnemyWithId enemy) HeroInterrupt
      (OwnsThis <> SelfMatches (IdentityWithTitle "Nightcrawler") <> SelfMatches IdentityCanDefend)
      (DiscardCost $ toTarget attrs) (runAbility a 1)]

instance RunMessage Nc48006 where
  runMessage msg a@(Nc48006 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      for_ (upgradeAttachedEnemy attrs) $ \enemy -> do
        declareHeroDefenseWithoutExhausting you enemy 0
        cardSpecific attrs "bamfDefense" (you, enemy)
      pure a
    CardSpecific (isTarget attrs -> True) "bamfDefense" (fromJSON -> Success (you :: IdentityId, enemy :: EnemyId)) -> do
      let card = MkPlayerCard (CardId $ unUpgradeId $ upgradeId attrs) (upgradeCardDef attrs) (Just $ upgradeOwner attrs) (Just $ upgradeController attrs)
      checkWindows [W.Window W.After $ W.DefendedWithBamf you enemy card]
      pure a
    _ -> Nc48006 <$> runMessage msg attrs
