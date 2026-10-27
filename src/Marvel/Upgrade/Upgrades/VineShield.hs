module Marvel.Upgrade.Upgrades.VineShield (
  vineShield,
  VineShield (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

vineShield :: UpgradeCard VineShield
vineShield = upgrade VineShield Cards.vineShield

newtype VineShield = VineShield (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: When Groot defends against an attack, remove 1 growth counter from him
-- and exhaust Vine Shield → Groot gets +3 DEF for that attack.
instance HasAbilities VineShield where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (HeroDefended When You AnyEnemy)
        HeroInterrupt
        (OwnsThis <> InHeroForm)
        (IdentityCountersCost 1 <> ExhaustCost)
        (runAbility a 1)
    ]

instance RunMessage VineShield where
  runMessage msg u@(VineShield attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      addToBasicPower ident 3
      pure u
    _ -> VineShield <$> runMessage msg attrs
