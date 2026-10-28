module Marvel.Ally.Allies.BladeEricBrooks (
  bladeEricBrooks,
  BladeEricBrooks (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Resource.Types (Resource (Physical))
import Marvel.Window hiding (AllyAttacked, AllyThwarted)
import Marvel.Window qualified as W

bladeEricBrooks :: AllyCard BladeEricBrooks
bladeEricBrooks = ally BladeEricBrooks Cards.bladeEricBrooks (Thw 1, 0) (Atk 2, 0) (HP 3)

newtype BladeEricBrooks = BladeEricBrooks (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Forced Response: After Blade thwarts or attacks, choose to either spend a
-- [physical] resource from your hand or discard Blade.
instance HasAbilities BladeEricBrooks where
  getAbilities a@(BladeEricBrooks attrs) =
    [ limitedWindowAbility
        a
        1
        (WindowOneOf [W.AllyThwarted After self AnyScheme, W.AllyAttacked After self AnyEnemy])
        ForcedResponse
        OwnsThis
        NoCost
        (runAbility a 1)
    ]
   where
    self = AllyWithId (allyId attrs)

instance RunMessage BladeEricBrooks where
  runMessage msg a@(BladeEricBrooks attrs) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      -- only resource cards from hand pay (orchestrator ruling)
      let cost = ResourcesFromHandCost (ResourceCost (Just Physical))
      canPay <- canAffordCost you (toSource attrs) cost
      chooseOrRunOne you $
        [labeled "$label.spendPhysicalFromHand" (payForTreachery you cost) | canPay]
          <> [labeled "$label.discardBlade" (removeFromPlay attrs)]
      pure a
    _ -> BladeEricBrooks <$> runMessage msg attrs
