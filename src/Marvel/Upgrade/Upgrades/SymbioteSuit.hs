module Marvel.Upgrade.Upgrades.SymbioteSuit where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

symbioteSuit :: UpgradeCard SymbioteSuit
symbioteSuit = upgrade SymbioteSuit Cards.symbioteSuit

newtype SymbioteSuit = SymbioteSuit (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SymbioteSuit where
  getModifiersFor _ (IdentityRef you) (SymbioteSuit attrs)
    | you == upgradeController attrs = pure
        [ AttackModifier 1, ThwartModifier 1, DefenseModifier 1
        , RecoveryModifier 1, HandSizeModifier 1, HitPointModifier 10
        ]
  getModifiersFor _ _ _ = pure []

instance RunMessage SymbioteSuit where
  runMessage msg (SymbioteSuit attrs) = runQueueT $ SymbioteSuit <$> runMessage msg attrs
