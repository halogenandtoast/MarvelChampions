{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Upgrade.Upgrades.AosP50044 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Keyword
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

aosP50044 :: UpgradeCard AosP50044
aosP50044 = upgrade AosP50044 Cards.aosP50044

newtype AosP50044 = AosP50044 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50044 where
  getAbilities (AosP50044 attrs) =
    [ability attrs n Resource OwnsThis (ExhaustCost <> CountersFromUpgradeCost (UpgradeWithKeyword SuitForm <> UpgradeControlledBy You) n) (Pay $ Payments $ replicate (fromIntegral n) $ ResourcePayment R.Mental) | n <- [1,2]]

instance RunMessage AosP50044 where
  runMessage msg (AosP50044 attrs) = runQueueT $ AosP50044 <$> runMessage msg attrs
