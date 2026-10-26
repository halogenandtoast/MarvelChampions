module Marvel.SideScheme.SideSchemes.MothersOrders (
  mothersOrders,
  MothersOrders (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

mothersOrders :: SideSchemeCard MothersOrders
mothersOrders = sideScheme MothersOrders Cards.mothersOrders (PerPlayer 2)

newtype MothersOrders = MothersOrders (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | As an additional cost for each hero to make a basic attack, that hero must spend 1 of
-- any resource. Folded into the basic attack ability's cost ('getAbilityCost'), so it is
-- paid with it, all at once (RR, Additional Costs), and the attack is not offered without
-- a resource to spend.
instance HasModifiersFor MothersOrders where
  getModifiersFor _ (IdentityRef _) _ = pure [BasicAttackAdditionalCost (ResourceCost Nothing)]
  getModifiersFor _ _ _ = pure []

instance RunMessage MothersOrders where
  runMessage msg (MothersOrders attrs) = runQueueT $ MothersOrders <$> runMessage msg attrs
