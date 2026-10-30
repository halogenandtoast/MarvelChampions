module Marvel.Upgrade.Upgrades.Unshakable where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Keyword (Keyword (Steady))

unshakable :: UpgradeCard Unshakable
unshakable = upgrade Unshakable Cards.unshakable

newtype Unshakable = Unshakable (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Unshakable where
  getAbilities _ = []

instance HasModifiersFor Unshakable where
  getModifiersFor _ target (Unshakable attrs) =
    pure [KeywordModifier Steady | target == IdentityRef (upgradeController attrs)]

instance RunMessage Unshakable where
  runMessage msg (Unshakable attrs) = Unshakable <$> runMessage msg attrs
