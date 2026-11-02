module Marvel.Upgrade.Upgrades.OutwitSilk where
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
import Marvel.Silk.Helpers

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Upgrade.Types

outwitSilk :: UpgradeCard OutwitSilk
outwitSilk = upgrade OutwitSilk Cards.outwitSilk
newtype OutwitSilk = OutwitSilk (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage OutwitSilk where
  runMessage msg u@(OutwitSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [scheme | W.MadeBasicThwartAgainst player scheme <- windows, player == you] $ \scheme -> do
        cards <- matchingTuckedCards you (schemeRef scheme)
        addToTargetedBasicThwart you (fromIntegral $ length cards)
      pure u
    _ -> OutwitSilk <$> runMessage msg attrs

instance HasAbilities OutwitSilk where
  getAbilities u@(OutwitSilk _attrs) = [limitedWindowAbility u 1 (W.MakesBasicThwartAgainst W.When You AnyScheme) HeroInterrupt OwnsThis ExhaustCost (runAbility u 1)]
