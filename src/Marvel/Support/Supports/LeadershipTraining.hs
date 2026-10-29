module Marvel.Support.Supports.LeadershipTraining (leadershipTraining, LeadershipTraining (..)) where

import Marvel.Prelude
import Marvel.Question
import Marvel.Ability
import Marvel.Aspect
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

leadershipTraining :: SupportCard LeadershipTraining
leadershipTraining = supportWith LeadershipTraining Cards.leadershipTraining $ (usesL .~ 2) . (discardIfNoUsesL .~ True)

newtype LeadershipTraining = LeadershipTraining (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

leadershipEvent :: IdentityMatcher -> ExtendedCardMatcher
leadershipEvent who = InDiscardOf who $ BasicCardMatches (CardWithType EventType <> CardWithAspect Leadership)

instance HasAbilities LeadershipTraining where
  getAbilities a = [ability a 1 AlterEgoAction
    (OwnsThis <> ExtendedCardExists (leadershipEvent You))
    (ExhaustCost <> UseCost) (runAbility a 1)]

instance RunMessage LeadershipTraining where
  runMessage msg s@(LeadershipTraining attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (leadershipEvent $ IdentityWithId you)
      unless (null cards) $ chooseOne you
        [targeting (toRef card) (shuffleIntoDeck you [card]) | card <- cards]
      pure s
    _ -> LeadershipTraining <$> runMessage msg attrs
