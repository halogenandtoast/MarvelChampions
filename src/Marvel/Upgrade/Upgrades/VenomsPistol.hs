module Marvel.Upgrade.Upgrades.VenomsPistol (
  venomsPistol,
  VenomsPistol (..),
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

venomsPistol :: UpgradeCard VenomsPistol
venomsPistol = upgrade VenomsPistol Cards.venomsPistol

newtype VenomsPistol = VenomsPistol (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: When you use one of Venom's basic powers (THW, ATK or DEF), exhaust
-- Venom's Pistol → Venom gets +1 to that power for this use. Each Pistol is its own +1.
instance HasAbilities VenomsPistol where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (WindowOneOf [MakesBasicThwart When You, MakesBasicAttack When You, HeroDefended When You AnyEnemy])
        HeroInterrupt
        OwnsThis
        ExhaustCost
        (runAbility a 1)
    ]

instance RunMessage VenomsPistol where
  runMessage msg u@(VenomsPistol attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      addToBasicPower ident 1
      pure u
    _ -> VenomsPistol <$> runMessage msg attrs
