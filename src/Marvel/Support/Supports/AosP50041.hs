{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50041 where

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

aosP50041 :: SupportCard AosP50041
aosP50041 = support AosP50041 Cards.aosP50041

newtype AosP50041 = AosP50041 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50041 where
  getAbilities (AosP50041 attrs) =
    [ability attrs 1 AlterEgoAction (OwnsThis) (ExhaustCost) (runAbility attrs 1)]

instance RunMessage AosP50041 where
  runMessage msg card@(AosP50041 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      canHeal <- identityMatches healableIdentity you
      chooseOne you $
        [labeled "$label.heal" (heal you 2) | canHeal]
        <> [labeled "$label.addThreat" (placeSuitThreat you 1)]
      pure card
    _ -> AosP50041 <$> runMessage msg attrs
