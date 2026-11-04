module Marvel.Upgrade.Upgrades.Distraction where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Trait qualified as Trait

distraction :: UpgradeCard Distraction
distraction = upgrade Distraction Cards.distraction

newtype Distraction = Distraction (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade Distraction where
  upgradeTargets _ = do
    candidates <- selectList (NotMinion $ MinionWithTrait Trait.Elite)
    minions <- filterM (\mid -> not <$> selectAny (EnemyWithId (toEnemy mid) <> EnemyWithUpgrade (UpgradeIs Cards.distraction))) candidates
    legal <- filterM (fmap (\modifiers -> CannotHaveAttachedCards `notElem` modifiers && CannotHaveAttachedPlayerCards `notElem` modifiers && CannotHaveAttachedUpgrades `notElem` modifiers) . getModifiers . MinionRef) minions
    pure $ Just $ map MinionRef legal

instance HasModifiersFor Distraction where
  getModifiersFor _ (UpgradeRef uid) (Distraction attrs) = pure [CrisisIcon | uid == upgradeId attrs]
  getModifiersFor _ target (Distraction attrs)
    | Just target == (enemyRef <$> upgradeAttachedEnemy attrs) = pure [CannotActivate]
  getModifiersFor _ _ _ = pure []

instance RunMessage Distraction where
  runMessage msg (Distraction attrs) = runQueueT $ case msg of
    _ -> Distraction <$> runMessage msg attrs
