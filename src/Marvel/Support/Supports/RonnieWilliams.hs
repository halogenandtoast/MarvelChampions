module Marvel.Support.Supports.RonnieWilliams where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)

ronnieWilliams :: SupportCard RonnieWilliams
ronnieWilliams = support RonnieWilliams Cards.ronnieWilliams

newtype RonnieWilliams = RonnieWilliams (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RonnieWilliams where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage RonnieWilliams where
  runMessage msg e@(RonnieWilliams attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      damaged <- identityMatches healableIdentity you
      chooseOne you $
        [labeled "$label.ronnieHeal" (heal you 2) | damaged]
        <> [labeled "$label.ronnieProgress" (placeCounters you 1)]
      pure e
    _ -> RonnieWilliams <$> runMessage msg attrs
