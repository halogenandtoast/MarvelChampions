module Marvel.Support.Supports.TheDailyBeagle where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Question
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

theDailyBeagle :: SupportCard TheDailyBeagle
theDailyBeagle = support TheDailyBeagle Cards.theDailyBeagle

newtype TheDailyBeagle = TheDailyBeagle (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TheDailyBeagle where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage TheDailyBeagle where
  runMessage msg s@(TheDailyBeagle attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> s <$ placeCounters you 1
    _ -> TheDailyBeagle <$> runMessage msg attrs
