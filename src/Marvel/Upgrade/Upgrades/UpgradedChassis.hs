module Marvel.Upgrade.Upgrades.UpgradedChassis (upgradedChassis, UpgradedChassis (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Trait (Trait (Aerial))
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

upgradedChassis :: UpgradeCard UpgradedChassis
upgradedChassis = upgrade UpgradedChassis Cards.upgradedChassis

newtype UpgradedChassis = UpgradedChassis (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor UpgradedChassis where
  getModifiersFor _ (IdentityRef ident) (UpgradedChassis attrs) | ident == upgradeController attrs = do
    hero <- selectAny (IdentityWithId ident <> HeroIdentity)
    pure [TraitModifier Aerial | hero]
  getModifiersFor _ _ _ = pure []

instance HasAbilities UpgradedChassis where
  getAbilities a@(UpgradedChassis attrs) =
    [limitedWindowAbility a 1
      (W.IdentityChangedToForm W.After $ IdentityWithId (upgradeController attrs) <> HeroIdentity)
      HeroResponse (OwnsThis <> SelfMatches IdentityWithoutToughStatus) ExhaustCost (runAbility a 1)]

instance RunMessage UpgradedChassis where
  runMessage msg u@(UpgradedChassis attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      becomeTough ident
      pure u
    _ -> UpgradedChassis <$> runMessage msg attrs
