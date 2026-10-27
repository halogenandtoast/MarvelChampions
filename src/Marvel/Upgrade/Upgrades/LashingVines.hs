module Marvel.Upgrade.Upgrades.LashingVines (
  lashingVines,
  LashingVines (..),
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

lashingVines :: UpgradeCard LashingVines
lashingVines = upgrade LashingVines Cards.lashingVines

newtype LashingVines = LashingVines (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response: After Groot uses a basic power, remove 2 growth counters from him and
-- exhaust Lashing Vines → ready Groot. A hero's basic powers are his attack, thwart and
-- defense; recovering is in alter-ego form, where a Hero Response is not used.
instance HasAbilities LashingVines where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (WindowOneOf [MakesBasicAttack After You, MakesBasicThwart After You, HeroDefended After You AnyEnemy])
        HeroResponse
        (OwnsThis <> InHeroForm)
        (IdentityCountersCost 2 <> ExhaustCost)
        (runAbility a 1)
    ]

instance RunMessage LashingVines where
  runMessage msg u@(LashingVines attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      ready ident
      pure u
    _ -> LashingVines <$> runMessage msg attrs
