module Marvel.Support.Supports.MoiraMacTaggert where

import Marvel.Question ( runAbility )
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier

import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Window qualified as W

moiraMacTaggert :: SupportCard MoiraMacTaggert
moiraMacTaggert = support MoiraMacTaggert Cards.moiraMacTaggert

newtype MoiraMacTaggert = MoiraMacTaggert (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MoiraMacTaggert where
  getAbilities a = [limitedWindowAbility a 1 (W.MutantChangesToHero W.After AnyIdentity) Response OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage MoiraMacTaggert where
  runMessage msg a@(MoiraMacTaggert attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [you | W.MutantChangedToHero you <- windows] $ \you -> drawCards you 1
      pure a
    _ -> MoiraMacTaggert <$> runMessage msg attrs
