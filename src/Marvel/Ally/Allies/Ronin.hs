module Marvel.Ally.Allies.Ronin (
  ronin,
  Ronin (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Modifier ()
import Marvel.Ref ()

ronin :: AllyCard Ronin
ronin = ally Ronin Cards.ronin (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Ronin = Ronin (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Ronin gets +1 THW and +1 ATK while an upgrade is attached to them.
instance HasModifiersFor Ronin where
  getModifiersFor _ (AllyRef aid) (Ronin a)
    | aid == allyId a && notNull (allyUpgrades a) = pure [ThwartModifier 1, AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage Ronin where
  runMessage msg (Ronin attrs) = Ronin <$> runMessage msg attrs
