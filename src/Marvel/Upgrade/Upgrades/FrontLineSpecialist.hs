module Marvel.Upgrade.Upgrades.FrontLineSpecialist where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

frontLineSpecialist :: UpgradeCard FrontLineSpecialist
frontLineSpecialist = upgrade FrontLineSpecialist Cards.frontLineSpecialist

newtype FrontLineSpecialist = FrontLineSpecialist (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities FrontLineSpecialist where
  getAbilities a = [limitedWindowAbility a 1 (W.TookDamage You FromAttack AnyEnemy) Response OwnsThis ExhaustCost (runAbility a 1)]

instance HasModifiersFor FrontLineSpecialist where
  getModifiersFor _ (IdentityRef you) (FrontLineSpecialist attrs) | you == upgradeController attrs = pure [HitPointModifier 4]
  getModifiersFor _ _ _ = pure []

instance RunMessage FrontLineSpecialist where
  runMessage msg a@(FrontLineSpecialist attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ drawCards you 1
    _ -> FrontLineSpecialist <$> runMessage msg attrs
