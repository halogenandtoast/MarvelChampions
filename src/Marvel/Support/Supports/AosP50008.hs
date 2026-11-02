{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50008 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
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
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

aosP50008 :: SupportCard AosP50008
aosP50008 = supportWith AosP50008 Cards.aosP50008 ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype AosP50008 = AosP50008 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50008 where
  getAbilities (AosP50008 attrs) =
    [ability attrs 1 Resource (ForAnyPlayer <> SelfMatches (IdentityWithTrait T.Shield)) (ExhaustCost <> UseCost) (Pay $ ResourcePayment R.Wild)]

instance RunMessage AosP50008 where
  runMessage msg (AosP50008 attrs) = runQueueT $ AosP50008 <$> runMessage msg attrs
