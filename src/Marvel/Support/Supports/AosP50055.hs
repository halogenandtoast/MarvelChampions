{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50055 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Id
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

aosP50055 :: SupportCard AosP50055
aosP50055 = support AosP50055 Cards.aosP50055

newtype AosP50055 = AosP50055 (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50055 where
  getAbilities (AosP50055 attrs) =
    [ability attrs 1 Resource (OwnsThis <> PayingFor (CardWithTrait T.Tech)) (ExhaustCost) (Pay $ ResourcePayment R.Mental)]

instance HasModifiersFor AosP50055 where
  getModifiersFor _ target (AosP50055 attrs) | target == CardIdRef (CardId $ unSupportId $ supportId attrs) = do
    shield <- identityMatches (IdentityWithTrait T.Shield) (supportController attrs)
    pure [ResourceCostReduction 2 | shield]
  getModifiersFor _ _ _ = pure []

instance RunMessage AosP50055 where
  runMessage msg (AosP50055 attrs) = runQueueT $ AosP50055 <$> runMessage msg attrs
