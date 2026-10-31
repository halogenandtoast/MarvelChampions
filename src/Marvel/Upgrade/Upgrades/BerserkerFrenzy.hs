module Marvel.Upgrade.Upgrades.BerserkerFrenzy where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Question
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

berserkerFrenzy :: UpgradeCard BerserkerFrenzy
berserkerFrenzy = upgrade BerserkerFrenzy Cards.berserkerFrenzy

newtype BerserkerFrenzy = BerserkerFrenzy (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BerserkerFrenzy where
  getAbilities a@(BerserkerFrenzy attrs) =
    [ limitedWindowAbility a 1 (W.TookDamage (IdentityWithId $ upgradeController attrs) damageSource AnyEnemy) HeroResponse OwnsThis NoCost (runAbility a 1)
    | damageSource <- [FromAttack, FromOverkill]
    ] <>
    [ limitedWindowAbility a 2 (W.IdentityChangedToForm W.After $ IdentityWithId $ upgradeController attrs) ForcedResponse (OwnsThis <> InAlterEgoForm) NoCost (runAbility a 2)
    ]

instance RunMessage BerserkerFrenzy where
  runMessage msg u@(BerserkerFrenzy attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ drawCards you 1
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> u <$ discardUpgrade (upgradeId attrs)
    _ -> BerserkerFrenzy <$> runMessage msg attrs
