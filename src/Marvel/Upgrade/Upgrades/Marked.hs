module Marvel.Upgrade.Upgrades.Marked where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Keyword (Keyword (Overkill))

marked :: UpgradeCard Marked
marked = upgrade Marked Cards.marked

newtype Marked = Marked (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade Marked where
  upgradeTargets _ = Just . map enemyRef <$> selectList
    (MinionEnemy <> NotEnemy (EnemyWithUpgrade $ UpgradeIs Cards.marked))

instance HasModifiersFor Marked where
  getModifiersFor _ target (Marked attrs) = pure
    [AttacksAgainstGainKeyword Overkill | Just target == (enemyRef <$> upgradeAttachedEnemy attrs)]

instance RunMessage Marked where
  runMessage msg (Marked attrs) = runQueueT $ Marked <$> runMessage msg attrs
