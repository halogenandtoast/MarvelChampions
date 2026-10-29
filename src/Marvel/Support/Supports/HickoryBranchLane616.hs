module Marvel.Support.Supports.HickoryBranchLane616 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Query
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

hickoryBranchLane616 :: SupportCard HickoryBranchLane616
hickoryBranchLane616 = support HickoryBranchLane616 Cards.hickoryBranchLane616

newtype HickoryBranchLane616 = HickoryBranchLane616 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities HickoryBranchLane616 where
  getAbilities a = [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage HickoryBranchLane616 where
  runMessage msg s@(HickoryBranchLane616 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeckAndDiscardPile you) (CardWithType AllyType <> CardWithTrait Trait.Android) SearchDrawOne ShuffleBackIn
      pure s
    _ -> HickoryBranchLane616 <$> runMessage msg attrs
