module Marvel.Upgrade.Upgrades.SpdrPilot where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

spdrPilot :: UpgradeCard SpdrPilot
spdrPilot = upgrade SpdrPilot Cards.spdrPilot

newtype SpdrPilot = SpdrPilot (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- The engine creates this permanent face when Peni suits up, retaining her
-- exhaustion; its printed wild is available through the Suit's Sync Ratio.
instance RunMessage SpdrPilot where
  runMessage msg (SpdrPilot attrs) = runQueueT $ SpdrPilot <$> runMessage msg attrs
