module Marvel.Support.Supports.Limbo where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Projection
import Marvel.Phase
import Marvel.Window qualified as W

limbo :: SupportCard Limbo
limbo = support Limbo Cards.limbo

newtype Limbo = Limbo (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Limbo where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.PhaseBegins W.After VillainPhase) Response valid ExhaustCost (runAbility a 1)
    , ability a 2 Action valid ExhaustCost (runAbility a 2)
    ]
   where
    valid = OwnsThis <> ExtendedCardExists (InHandOf You $ BasicCardMatches AnyCard)
      <> ExtendedCardExists (InDeckOf You $ BasicCardMatches AnyCard)

instance RunMessage Limbo where
  runMessage msg a@(Limbo attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) n _ _ | n `elem` [1, 2] -> do
      hand <- projectMap PlayerIdentityHand unHand you
      chooseOne you [targeting card (cardSpecific you "swapHandWithDeckTop" card) | card <- hand]
      pure a
    _ -> Limbo <$> runMessage msg attrs
