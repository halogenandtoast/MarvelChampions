{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw53011 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Upgrade.Types

import Marvel.Phase
fw53011 :: UpgradeCard Fw53011
fw53011 = upgrade Fw53011 Cards.fw53011

newtype Fw53011 = Fw53011 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53011 where
  getAbilities a@(Fw53011 attrs) =
    [limitedWindowAbility a 1 (W.PhaseBegins W.After VillainPhase) HeroResponse (OwnsThis) (DiscardCost $ toTarget attrs) (runAbility a 1)]

instance RunMessage Fw53011 where
  runMessage msg a@(Fw53011 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      lastingModifiers attrs (IdentityEntity $ IdentityWithId you) [DefendWithoutExhausting] False
      pure a
    _ -> Fw53011 <$> runMessage msg attrs
