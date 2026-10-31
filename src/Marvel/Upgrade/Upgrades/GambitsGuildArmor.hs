module Marvel.Upgrade.Upgrades.GambitsGuildArmor where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Matchers qualified as Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

gambitsGuildArmor :: UpgradeCard GambitsGuildArmor
gambitsGuildArmor = upgrade GambitsGuildArmor Cards.gambitsGuildArmor

newtype GambitsGuildArmor = GambitsGuildArmor (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GambitsGuildArmor where
  getAbilities a@(GambitsGuildArmor attrs) =
    [limitedWindowAbility a 1 (W.HeroDefendedWithoutDamage W.After $ IdentityWithId $ upgradeController attrs)
      HeroResponse (OwnsThis <> SelfMatches (IdentityWithTitle "Gambit" <> Matchers.ExhaustedIdentity)) ExhaustCost (runAbility a 1)]

instance RunMessage GambitsGuildArmor where
  runMessage msg u@(GambitsGuildArmor attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ ready you
    _ -> GambitsGuildArmor <$> runMessage msg attrs
