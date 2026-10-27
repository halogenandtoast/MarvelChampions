module Marvel.Event.Events.GraspingTendrils (
  graspingTendrils,
  GraspingTendrils (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

graspingTendrils :: EventCard GraspingTendrils
graspingTendrils = event GraspingTendrils Cards.graspingTendrils

newtype GraspingTendrils = GraspingTendrils (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt (defense): when the villain initiates an attack against you, cancel
-- that attack (you defended it, RR FAQ). Paid for using only [physical] resources, stun
-- the villain.
instance RunMessage GraspingTendrils where
  runMessage msg e@(GraspingTendrils attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment (Just (W.EnemyAttack enemy _))) | ident == eventId attrs -> do
      physical <- paidUsingOnly Physical payment
      cancelAttackAsDefense you enemy
      when physical $ stun enemy attrs
      pure e
    _ -> GraspingTendrils <$> runMessage msg attrs
