module Marvel.Upgrade.Upgrades.BattleFury (
  battleFury,
  BattleFury (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

battleFury :: UpgradeCard BattleFury
battleFury = upgrade BattleFury Cards.battleFury

newtype BattleFury = BattleFury (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Your hero" is the hero of the player controlling it.
instance HasAbilities BattleFury where
  getAbilities (BattleFury a) =
    [ limitedWindowAbility
        a
        1
        (W.EnemyDefeated W.After MinionEnemy (AttackFromPlayer $ IdentityWithId $ upgradeController a))
        Response
        OwnsThis
        (DamageCost 1 <> DiscardCost (toTarget a))
        ReadyIdentity
    ]

instance RunMessage BattleFury where
  runMessage msg (BattleFury attrs) = BattleFury <$> runMessage msg attrs
