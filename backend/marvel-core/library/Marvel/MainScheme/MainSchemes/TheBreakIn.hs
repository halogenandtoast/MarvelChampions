module Marvel.MainScheme.MainSchemes.TheBreakIn where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.MainScheme.Types
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target

theBreakIn :: MainSchemeCard TheBreakIn
theBreakIn =
  mainScheme TheBreakIn Cards.theBreakIn (PerPlayer 7) (Static 0) (PerPlayer 1)

newtype TheBreakIn = TheBreakIn (Attrs MainScheme)
  deriving anyclass IsMainScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage TheBreakIn where
  runMessage msg ms@(TheBreakIn attrs) = case msg of
    AdvanceMainScheme -> ms <$ push (GameOver Lost)
    _ -> TheBreakIn <$> runMessage msg attrs
