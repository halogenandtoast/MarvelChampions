module Marvel.Ally.Allies.Lockjaw (
  lockjaw,
  Lockjaw (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

-- | Played from the discard pile as from hand: see 'cdPlayableFromDiscard'.
lockjaw :: AllyCard Lockjaw
lockjaw = ally Lockjaw Cards.lockjaw (Thw 2, 2) (Atk 2, 2) (HP 4)

newtype Lockjaw = Lockjaw (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Lockjaw where
  runMessage msg (Lockjaw a) = Lockjaw <$> runMessage msg a
