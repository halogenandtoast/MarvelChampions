module Marvel.Support.Supports.GankeLee where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)

gankeLee :: SupportCard GankeLee
gankeLee = support GankeLee Cards.gankeLee

newtype GankeLee = GankeLee (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GankeLee where
  getAbilities a = [ability a 1 Action OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage GankeLee where
  runMessage msg s@(GankeLee attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      drawCards you 1
      cardSpecific attrs "discardInHeroForm" you
      pure s
    CardSpecific (isTarget attrs -> True) "discardInHeroForm" (fromJSON -> Success you) -> do
      hero <- identityMatches HeroIdentity you
      when hero $ discardFromHand you 1
      pure s
    _ -> GankeLee <$> runMessage msg attrs
