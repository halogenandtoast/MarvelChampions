module Marvel.Ally.Allies.QuakeDaisyJohnson (
  quakeDaisyJohnson,
  QuakeDaisyJohnson (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Window

quakeDaisyJohnson :: AllyCard QuakeDaisyJohnson
quakeDaisyJohnson =
  ally QuakeDaisyJohnson Cards.quakeDaisyJohnson (Thw 1, 1) (Atk 2, 1) (HP 2)

newtype QuakeDaisyJohnson = QuakeDaisyJohnson (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities QuakeDaisyJohnson where
  getAbilities a =
    [ limitedWindowAbility a 1 (MinionHasSchemed After AnyMinion) Response OwnsThis ExhaustCost $
        runAbility a 1
    ]

-- | Not an attack: 2 damage to the minion that schemed.
instance RunMessage QuakeDaisyJohnson where
  runMessage msg a@(QuakeDaisyJohnson attrs) = case msg of
    RanAbility _ (isTarget a -> True) 1 [SchemedByMinion minion] _ -> do
      push $ MinionMessage minion $ MinionDamaged (toSource attrs) (toDamage 2 FromAbility)
      pure a
    _ -> QuakeDaisyJohnson <$> runMessage msg attrs
