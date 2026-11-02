{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Upgrade.Upgrades.AosP50011 where

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

aosP50011 :: UpgradeCard AosP50011
aosP50011 = upgrade AosP50011 Cards.aosP50011

newtype AosP50011 = AosP50011 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50011 where
  getAbilities (AosP50011 attrs) =
    [ability attrs 1 Action (OwnsThis <> SupportExists (SupportWithTrait T.Shield)) ExhaustCost (runAbility attrs 1)]

instance RunMessage AosP50011 where
  runMessage msg card@(AosP50011 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> card <$ chooseShieldCounter you attrs
    _ -> AosP50011 <$> runMessage msg attrs
