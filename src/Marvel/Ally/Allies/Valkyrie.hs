module Marvel.Ally.Allies.Valkyrie (
  valkyrie,
  Valkyrie (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Resource
import Marvel.Window

valkyrie :: AllyCard Valkyrie
valkyrie = ally Valkyrie Cards.valkyrie (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Valkyrie = Valkyrie (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Valkyrie where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) Response (OwnsThis <> MinionExists AnyMinion) NoCost $
        runAbility a 1
    ]

instance RunMessage Valkyrie where
  runMessage msg a@(Valkyrie attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      energy <- paidWith Energy (allyPaidWith attrs)
      let n = if energy then 3 else 2
      chooseDamage ident a (toDamage n FromAbility) MinionEnemy
      pure a
    _ -> Valkyrie <$> runMessage msg attrs
