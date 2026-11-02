{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw53023 where
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

import Marvel.FalconWinter.Helpers
fw53023 :: UpgradeCard Fw53023
fw53023 = upgrade Fw53023 Cards.fw53023

newtype Fw53023 = Fw53023 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53023 where
  getAbilities a@(Fw53023 attrs) =
    [ability a 1 HeroAction OwnsThis (ExhaustCost <> ResourceCost (Just R.Physical)) (runAbility a 1)]

instance RunMessage Fw53023 where
  runMessage msg a@(Fw53023 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      findPlayerCardToHand you attrs Cards.fw53034 4
      pure a
    _ -> Fw53023 <$> runMessage msg attrs
