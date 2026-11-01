module Marvel.Support.Supports.PlotConvenience where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Aspect
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Projection
import Marvel.Question (runAbility)
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

plotConvenience :: SupportCard PlotConvenience
plotConvenience = support PlotConvenience Cards.plotConvenience

newtype PlotConvenience = PlotConvenience (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

aspectCard :: CardMatcher
aspectCard = CardMatchesAny [CardWithAspect aspect | aspect <- [Aggression, Justice, Leadership, Protection, Pool]]

instance HasAbilities PlotConvenience where
  getAbilities a@(PlotConvenience attrs) =
    [ability a 1 Action (ForAnyPlayer <> AnyCriteria options) ExhaustCost (runAbility a 1)]
   where
    options = [ExtendedCardExists (InHandOf You $ BasicCardMatches aspectCard) | length (supportHeldCards attrs) < 3]
      <> [NoCriteria | notNull (supportHeldCards attrs)]

instance RunMessage PlotConvenience where
  runMessage msg e@(PlotConvenience attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch aspectCard) <$> projectMap PlayerIdentityHand unHand you
      chooseOne you $
        [ labeled "$label.plotConvenienceStore" $
            chooseOne you [targeting card (holdFacedownCard attrs card) | card <- cards]
        | length (supportHeldCards attrs) < 3, notNull cards]
        <> [ labeled "$label.plotConvenienceRetrieve" $
              chooseOne you [targeting card (releaseHeldCard attrs you card) | card <- supportHeldCards attrs]
           | notNull (supportHeldCards attrs)]
      pure e
    _ -> PlotConvenience <$> runMessage msg attrs
