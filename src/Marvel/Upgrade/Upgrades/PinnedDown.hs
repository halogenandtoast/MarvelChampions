module Marvel.Upgrade.Upgrades.PinnedDown where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

pinnedDown :: UpgradeCard PinnedDown
pinnedDown = upgrade PinnedDown Cards.pinnedDown

newtype PinnedDown = PinnedDown (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade PinnedDown where
  upgradeTargets _ = Just . map enemyRef <$> selectList
    (MinionEnemy <> NotEnemy (EnemyWithUpgrade $ UpgradeIs Cards.pinnedDown))

instance HasModifiersFor PinnedDown where
  getModifiersFor _ target (PinnedDown attrs) = pure
    [AttackPenalty 2 | Just target == (enemyRef <$> upgradeAttachedEnemy attrs)]

instance RunMessage PinnedDown where
  runMessage msg (PinnedDown attrs) = runQueueT $ PinnedDown <$> runMessage msg attrs
