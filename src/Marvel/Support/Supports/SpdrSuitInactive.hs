module Marvel.Support.Supports.SpdrSuitInactive where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

spdrSuitInactive :: SupportCard SpdrSuitInactive
spdrSuitInactive = support SpdrSuitInactive Cards.spdrSuitInactive

newtype SpdrSuitInactive = SpdrSuitInactive (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- The paired identity flip is performed atomically by the engine, preserving
-- this physical card's exhaustion and transferring the shared identity state.
instance RunMessage SpdrSuitInactive where
  runMessage msg (SpdrSuitInactive attrs) = runQueueT $ SpdrSuitInactive <$> runMessage msg attrs
