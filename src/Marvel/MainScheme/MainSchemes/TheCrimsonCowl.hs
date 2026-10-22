module Marvel.MainScheme.MainSchemes.TheCrimsonCowl where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.EncounterCard
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query

theCrimsonCowl :: MainSchemeCard TheCrimsonCowl
theCrimsonCowl =
  mainScheme TheCrimsonCowl Cards.theCrimsonCowl (PerPlayer 3) (Static 0) (PerPlayer 1)

newtype TheCrimsonCowl = TheCrimsonCowl (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage TheCrimsonCowl where
  runMessage msg ms@(TheCrimsonCowl attrs) = runQueueT $ case msg of
    -- 1B: each player puts the top card of their deck into play as a Drone
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      players <- getPlayers
      for_ players putTopOfDeckIntoPlayAsDrone
      pure ms
    -- 1A: put the Ultron Drones environment into play and shuffle the encounter deck
    SetupMainScheme -> do
      firstPlayer <- selectJust FirstPlayer
      drones <- genEncounterCard Cards.ultronDrones
      revealEncounterCardWithoutWindows firstPlayer drones
      shuffleEncounterDeck
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ nextMainScheme (mainSchemeId attrs)
    _ -> TheCrimsonCowl <$> runMessage msg attrs
