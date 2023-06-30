module Marvel.MainScheme.MainSchemes.TheBreakIn where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message
import Marvel.Queue

theBreakIn :: MainSchemeCard TheBreakIn
theBreakIn =
  mainScheme TheBreakIn Cards.theBreakIn (PerPlayer 7) (Static 0) (PerPlayer 1)

newtype TheBreakIn = TheBreakIn (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage TheBreakIn where
  runMessage msg ms@(TheBreakIn attrs) = case msg of
    AdvanceMainScheme -> ms <$ push (GameOver Lost)
    _ -> TheBreakIn <$> runMessage msg attrs
