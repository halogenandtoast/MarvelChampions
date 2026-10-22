module Marvel.Ally.Allies.Elektra (
  elektra,
  Elektra (..),
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

elektra :: AllyCard Elektra
elektra = ally Elektra Cards.elektra (Thw 1, 1) (Atk 3, 1) (HP 3)

newtype Elektra = Elektra (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | A Captive ally: "Response: After you play Elektra from your hand, spend a [Physical]
-- resource → deal 3 damage to an enemy."
instance HasAbilities Elektra where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) Response OwnsThis (ResourceCost $ Just Physical) $
        runAbility a 1
    ]

instance RunMessage Elektra where
  runMessage msg a@(Elektra attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      chooseDamage ident a (toDamage 3 FromAbility) AnyEnemy
      pure a
    _ -> Elektra <$> runMessage msg attrs
