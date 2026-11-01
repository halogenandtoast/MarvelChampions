module Marvel.Support.Supports.GovernmentLiaison where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Game.Source (canPlayCardWithReduction)
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Projection
import Marvel.Matchers ()
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Trait (Trait (Shield))

governmentLiaison :: SupportCard GovernmentLiaison
governmentLiaison = support GovernmentLiaison Cards.governmentLiaison

newtype GovernmentLiaison = GovernmentLiaison (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GovernmentLiaison where
  getAbilities a = [ability a 1 HeroAction (OwnsThis <> PlayCardWithReductionHasTarget (CardWithTrait Shield) 1) ExhaustCost (runAbility a 1)]

instance RunMessage GovernmentLiaison where
  runMessage msg s@(GovernmentLiaison attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      hand <- projectMap PlayerIdentityHand unHand you
      cards <- filterM (canPlayCardWithReduction you 1) (filter (cardMatch $ CardWithTrait Shield) hand)
      unless (null cards) $ chooseOne you [targeting card (playCardWithReduction you 1 card) | card <- cards]
      pure s
    _ -> GovernmentLiaison <$> runMessage msg attrs
