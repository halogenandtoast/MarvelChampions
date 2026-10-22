module Marvel.Ally.Allies.RedDagger (
  redDagger,
  RedDagger (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers hiding (ExhaustedAlly)
import Marvel.Window qualified as W

redDagger :: AllyCard RedDagger
redDagger = ally RedDagger Cards.redDagger (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype RedDagger = RedDagger (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities RedDagger where
  getAbilities a@(RedDagger attrs) =
    [ limitedWindowAbility
        a
        1
        (W.AllyDefeated W.When $ AllyWithId $ allyId attrs)
        Interrupt
        OwnsThis
        (DifferentResourcesCost 2)
        (runAbility a 1)
    ]

-- | Defeated, she goes to her owner's hand instead of the discard pile.
instance RunMessage RedDagger where
  runMessage msg a@(RedDagger attrs) = case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      let self = toTarget a
      replaceMatchingMessage (const [ReturnToHand self]) (== RemoveFromPlay self)
      pushChoice ident $ ChooseDamage (toSource a) (toDamage 2 FromAbility) AnyEnemy
      pure a
    _ -> RedDagger <$> runMessage msg attrs
