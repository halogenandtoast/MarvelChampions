module Marvel.Support.Supports.StepfordCuckoos where

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
import Marvel.Matchers
import Marvel.Window qualified as W

stepfordCuckoos :: SupportCard StepfordCuckoos
stepfordCuckoos = supportWith StepfordCuckoos Cards.stepfordCuckoos ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype StepfordCuckoos = StepfordCuckoos (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities StepfordCuckoos where
  getAbilities a = [limitedWindowAbility a 1
    (W.EncounterCardRevealedBy W.When AnyIdentity $ EncounterCardMatching $ CardWithType TreacheryType)
    Interrupt (OwnsThis <> CanCancelRevealedEncounterCard) (ExhaustCost <> UseCost) (runAbility a 1)]

instance RunMessage StepfordCuckoos where
  runMessage msg a@(StepfordCuckoos attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [(you, card) | W.EncounterCardRevealed _ you card <- windows] \(you, card) -> do
        cancelEncounterCard card
        drawAndRevealEncounterCard you
      pure a
    _ -> StepfordCuckoos <$> runMessage msg attrs
