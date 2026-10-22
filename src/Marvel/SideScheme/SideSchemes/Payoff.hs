module Marvel.SideScheme.SideSchemes.Payoff (
  payoff,
  Payoff (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Environment.Environments.CriminalEnterprise (infamyOrMadness)

payoff :: SideSchemeCard Payoff
payoff = sideScheme Payoff Cards.payoff (PerPlayer 2)

newtype Payoff = Payoff (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Boost: place 1 infamy counter on Criminal Enterprise, or, if you cannot, remove 1
-- madness counter from State of Madness.
instance RunMessage Payoff where
  runMessage msg s@(Payoff attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target _) | isTarget attrs target -> s <$ infamyOrMadness 1
    _ -> Payoff <$> runMessage msg attrs
