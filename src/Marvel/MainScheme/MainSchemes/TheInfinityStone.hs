module Marvel.MainScheme.MainSchemes.TheInfinityStone where

import Marvel.Prelude

import Marvel.Area (experimentalWeaponsDeck)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query

theInfinityStone :: MainSchemeCard TheInfinityStone
theInfinityStone =
  mainScheme TheInfinityStone Cards.theInfinityStone (PerPlayer 6) (PerPlayer 1) (PerPlayer 1)

newtype TheInfinityStone = TheInfinityStone (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: reveal the top card of the Experimental Weapons deck.
instance RunMessage TheInfinityStone where
  runMessage msg ms@(TheInfinityStone attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      you <- selectJust FirstPlayer
      fromDeck experimentalWeaponsDeck (drawAndRevealEncounterCard you)
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ nextMainScheme (mainSchemeId attrs)
    _ -> TheInfinityStone <$> runMessage msg attrs
