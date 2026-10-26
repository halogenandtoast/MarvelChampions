module Marvel.SideScheme.SideSchemes.TheNextEvolution (
  theNextEvolution,
  TheNextEvolution (..),
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

theNextEvolution :: SideSchemeCard TheNextEvolution
theNextEvolution = sideScheme TheNextEvolution Cards.theNextEvolution (PerPlayer 2)

newtype TheNextEvolution = TheNextEvolution (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Increase the number of boost icons on each encounter card by 1.
instance HasModifiersFor TheNextEvolution where
  getModifiersFor _ GameRef _ = pure [AdditionalBoostIcons 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheNextEvolution where
  runMessage msg (TheNextEvolution attrs) = TheNextEvolution <$> runMessage msg attrs
