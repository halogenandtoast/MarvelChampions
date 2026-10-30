module Marvel.Upgrade.Upgrades.FluidMotion where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Effect.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

fluidMotion :: UpgradeCard FluidMotion
fluidMotion = upgrade FluidMotion Cards.fluidMotion

newtype FluidMotion = FluidMotion (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities FluidMotion where
  getAbilities a@(FluidMotion attrs) =
    [ limited (SharedPerWindow "fluid-motion" 1) $
        limitedWindowAbility a 1
          (W.PlaysEvent W.After (IdentityWithId $ upgradeController attrs) $ CardWithTrait Trait.Attack)
          HeroResponse OwnsThis ExhaustCost (runAbility a 1)
    ]

instance RunMessage FluidMotion where
  runMessage msg a@(FluidMotion attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      createEffect Cards.fluidMotion attrs (IdentityEntity $ IdentityWithId you)
      pure a
    _ -> FluidMotion <$> runMessage msg attrs

newtype FluidMotionEffect = FluidMotionEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

fluidMotionEffect :: CardEffect FluidMotionEffect
fluidMotionEffect = effectWith FluidMotionEffect Cards.fluidMotion $
  (modifiersL .~ [AttackModifier 1]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage FluidMotionEffect where
  runMessage msg (FluidMotionEffect attrs) = FluidMotionEffect <$> runMessage msg attrs
