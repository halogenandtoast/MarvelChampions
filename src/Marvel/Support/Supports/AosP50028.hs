{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50028 where

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

aosP50028 :: SupportCard AosP50028
aosP50028 = support AosP50028 Cards.aosP50028

newtype AosP50028 = AosP50028 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50028 where
  getAbilities (AosP50028 attrs) =
    [limitedWindowAbility attrs 1 (W.PlayerCardWouldBeDiscardedByEncounter $ IdentityWithId $ supportController attrs) Interrupt OwnsThis (DiscardCost $ toRef attrs) (runAbility attrs 1)]

instance RunMessage AosP50028 where
  runMessage msg card@(AosP50028 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [card | W.WouldBeDiscardedFromPlay (Left card) <- windows] preventCardDiscardFromPlay
      pure card
    _ -> AosP50028 <$> runMessage msg attrs
