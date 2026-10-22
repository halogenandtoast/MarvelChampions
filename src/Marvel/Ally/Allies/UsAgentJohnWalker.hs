module Marvel.Ally.Allies.UsAgentJohnWalker (
  usAgentJohnWalker,
  UsAgentJohnWalker (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria

usAgentJohnWalker :: AllyCard UsAgentJohnWalker
usAgentJohnWalker = ally UsAgentJohnWalker Cards.usAgentJohnWalker (Thw 1, 1) (Atk 1, 1) (HP 5)

-- | Retaliate 1, answered as he is attacked.
newtype UsAgentJohnWalker = UsAgentJohnWalker (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage UsAgentJohnWalker where
  runMessage msg (UsAgentJohnWalker attrs) = UsAgentJohnWalker <$> runMessage msg attrs
