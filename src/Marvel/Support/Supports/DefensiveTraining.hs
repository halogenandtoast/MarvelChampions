module Marvel.Support.Supports.DefensiveTraining (defensiveTraining, DefensiveTraining (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Aspect
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

protectionEvent :: CardMatcher
protectionEvent = CardWithType EventType <> CardWithAspect Protection

defensiveTraining :: SupportCard DefensiveTraining
defensiveTraining = supportWith DefensiveTraining Cards.defensiveTraining $
  (usesL .~ 2) . (discardIfNoUsesL .~ True)

newtype DefensiveTraining = DefensiveTraining (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DefensiveTraining where
  getAbilities a@(DefensiveTraining attrs) =
    [ability a 1 AlterEgoAction
      (OwnsThis <> ExtendedCardExists (InDiscardOf (IdentityWithId $ supportController attrs) $ BasicCardMatches protectionEvent))
      (ExhaustCost <> UseCost) (runAbility a 1)]

instance RunMessage DefensiveTraining where
  runMessage msg a@(DefensiveTraining attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId ident) $ BasicCardMatches protectionEvent)
      unless (null cards) $ chooseOne ident [targeting card (shuffleIntoDeck ident [card]) | card <- cards]
      pure a
    _ -> DefensiveTraining <$> runMessage msg attrs
