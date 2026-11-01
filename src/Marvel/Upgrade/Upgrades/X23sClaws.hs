module Marvel.Upgrade.Upgrades.X23sClaws where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Effect.Types
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

x23sClaws :: UpgradeCard X23sClaws
x23sClaws = upgrade X23sClaws Cards.x23sClaws

newtype X23sClaws = X23sClaws (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities X23sClaws where
  getAbilities a = [ability a 1 HeroAction OwnsThis (ExhaustCost <> DamageCost 2) (runAbility a 1)]

instance RunMessage X23sClaws where
  runMessage msg a@(X23sClaws attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      createEffect Cards.x23sClaws attrs (IdentityEntity $ IdentityIs Heroes.x23)
      pure a
    _ -> X23sClaws <$> runMessage msg attrs

newtype X23sClawsEffect = X23sClawsEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

x23sClawsEffect :: CardEffect X23sClawsEffect
x23sClawsEffect = effectWith X23sClawsEffect Cards.x23sClaws $
  (modifiersL .~ [AttackModifier 2]) . (endsL ?~ DisableAtEndOfRound)

instance RunMessage X23sClawsEffect where
  runMessage msg (X23sClawsEffect attrs) = X23sClawsEffect <$> runMessage msg attrs
