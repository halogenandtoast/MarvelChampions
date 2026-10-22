module Marvel.MainScheme.MainSchemes.TheGetaway where

import Marvel.Prelude

import Marvel.Area (experimentalWeaponsDeck)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query

theGetaway :: MainSchemeCard TheGetaway
theGetaway =
  mainScheme TheGetaway Cards.theGetaway (PerPlayer 5) (PerPlayer 1) (PerPlayer 1)

newtype TheGetaway = TheGetaway (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: reveal the top card of the Experimental Weapons deck. If this stage is
-- completed, the players lose the game.
instance RunMessage TheGetaway where
  runMessage msg ms@(TheGetaway attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      you <- selectJust FirstPlayer
      fromDeck experimentalWeaponsDeck (drawAndRevealEncounterCard you)
      pure ms
    AdvanceMainScheme -> ms <$ gameOver Lost
    _ -> TheGetaway <$> runMessage msg attrs
