module Marvel.Upgrade.Upgrades.Sidearm (sidearm, Sidearm (..)) where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Keyword (Keyword (Ranged))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

sidearm :: UpgradeCard Sidearm
sidearm = upgrade Sidearm Cards.sidearm

newtype Sidearm = Sidearm (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade Sidearm where
  upgradeTargets _ = Just . map toTarget <$> selectList (NotAlly $ AllyWithUpgrade $ UpgradeIs Cards.sidearm)

instance HasModifiersFor Sidearm where
  getModifiersFor _ (AllyRef aid) (Sidearm attrs)
    | Just aid == upgradeAttachedAlly attrs = pure [AttackModifier 1, KeywordModifier Ranged]
  getModifiersFor _ _ _ = pure []

instance RunMessage Sidearm where
  runMessage msg (Sidearm attrs) = runQueueT $ Sidearm <$> runMessage msg attrs
