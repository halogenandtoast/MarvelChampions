module Marvel.Upgrade.Upgrades.EntanglingVines (
  entanglingVines,
  EntanglingVines (..),
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

entanglingVines :: UpgradeCard EntanglingVines
entanglingVines = upgrade EntanglingVines Cards.entanglingVines

newtype EntanglingVines = EntanglingVines (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: When Groot makes a basic thwart, remove 1 growth counter from him and
-- exhaust Entangling Vines → Groot gets +2 THW for that thwart.
instance HasAbilities EntanglingVines where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (MakesBasicThwart When You)
        HeroInterrupt
        (OwnsThis <> InHeroForm)
        (IdentityCountersCost 1 <> ExhaustCost)
        (runAbility a 1)
    ]

instance RunMessage EntanglingVines where
  runMessage msg u@(EntanglingVines attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      addToBasicPower ident 2
      pure u
    _ -> EntanglingVines <$> runMessage msg attrs
