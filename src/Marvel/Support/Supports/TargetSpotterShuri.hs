module Marvel.Support.Supports.TargetSpotterShuri where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Support.Types

targetSpotterShuri :: SupportCard TargetSpotterShuri
targetSpotterShuri = supportWith TargetSpotterShuri Cards.targetSpotterShuri ((usesL .~ 2) . (discardIfNoUsesL .~ True))
newtype TargetSpotterShuri = TargetSpotterShuri (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage TargetSpotterShuri where
  runMessage msg u@(TargetSpotterShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [mid | W.EngagedMinion _ mid <- windows] $ \mid -> do
        lastingModifiers attrs (MinionEntity $ MinionWithId mid) [CannotActivate] False
        redirectPendingMinionEngagement mid you
      pure u
    _ -> TargetSpotterShuri <$> runMessage msg attrs

instance HasAbilities TargetSpotterShuri where
  getAbilities u@(TargetSpotterShuri _attrs) = [limitedWindowAbility u 1 (W.EngagesMinion W.When AnyIdentity AnyMinion) Interrupt OwnsThis UseCost (runAbility u 1)]
