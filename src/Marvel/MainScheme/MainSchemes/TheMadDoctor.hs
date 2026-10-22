module Marvel.MainScheme.MainSchemes.TheMadDoctor where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.MainSchemes.TheIslandOfDrZola (testCounters)
import Marvel.MainScheme.Types
import Marvel.Message.Lifted

theMadDoctor :: MainSchemeCard TheMadDoctor
theMadDoctor =
  mainScheme TheMadDoctor Cards.theMadDoctor (PerPlayer 8) (PerPlayer 1) (PerPlayer 1)

newtype TheMadDoctor = TheMadDoctor (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 2A: each player searches the encounter deck and discard pile for a minion and
-- reveals it. Completed, the players lose.
instance RunMessage TheMadDoctor where
  runMessage msg ms@(TheMadDoctor attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      players <- getPlayers
      for_ players \p -> searchAndRevealEncounterCard p (CardWithType MinionType)
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> TheMadDoctor <$> testCounters msg attrs
