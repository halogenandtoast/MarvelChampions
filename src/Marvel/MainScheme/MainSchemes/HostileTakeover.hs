module Marvel.MainScheme.MainSchemes.HostileTakeover where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.EncounterCard
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Environments.CriminalEnterprise (getInfamy, placeInfamy)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref

hostileTakeover :: MainSchemeCard HostileTakeover
hostileTakeover =
  mainScheme HostileTakeover Cards.hostileTakeover (PerPlayer 7) (PerPlayer 2) (PerPlayer 1)

newtype HostileTakeover = HostileTakeover (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HostileTakeover where
  runMessage msg ms@(HostileTakeover attrs) = runQueueT $ case msg of
    -- 1A: put the Criminal Enterprise environment into play and shuffle the encounter deck
    SetupMainScheme -> do
      firstPlayer <- selectJust FirstPlayer
      enterprise <- genEncounterCard Cards.criminalEnterprise
      revealEncounterCardWithoutWindows firstPlayer enterprise
      shuffleEncounterDeck
      pure ms
    -- 1B, When Completed: 1[per player] infamy, then a card from each player's deck
    -- for each infamy counter there is
    AdvanceMainScheme -> do
      n <- fromGameValue (PerPlayer 1)
      _ <- placeInfamy (fromIntegral n)
      cardSpecific attrs "discard" ()
      nextMainScheme
      pure ms
    CardSpecific (isTarget attrs -> True) "discard" _ -> do
      infamy <- getInfamy
      players <- getPlayers
      when (infamy > 0) $ for_ players \p -> discardTopOfDeck p infamy
      pure ms
    _ -> HostileTakeover <$> runMessage msg attrs
