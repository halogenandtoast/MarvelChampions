module Marvel.MainScheme.MainSchemes.MojoOpening where

import Marvel.Card
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Entity
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.MojoOpening
import Marvel.Prelude
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Sides
import Marvel.Trait qualified as Trait
import Marvel.Treachery.Cards qualified as Treacheries
import System.Random.Shuffle (shuffleM)

newtype MojoOpeningMainScheme = MojoOpeningMainScheme (Attrs MainScheme)
  deriving anyclass IsMainScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39002b, mojo39015b :: MainSchemeCard MojoOpeningMainScheme
mojo39002b = mainScheme MojoOpeningMainScheme Cards.mojo39002b (PerPlayer 6) (PerPlayer 1) (PerPlayer 2)
mojo39015b = mainScheme MojoOpeningMainScheme Cards.mojo39015b (PerPlayer 15) (PerPlayer 1) (PerPlayer 1)

instance RunMessage MojoOpeningMainScheme where
  runMessage msg e@(MojoOpeningMainScheme a) = runQueueT $ case msg of
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      if toCardCode a == "39002b"
        then traverse_ (\def -> putEncounterCardIntoPlay first =<< genEncounterCard def) [Environments.mojo39003a, Environments.mojo39004a]
        else do
          putEncounterCardIntoPlay first =<< genEncounterCard Sides.mojo39016
          deck <- getEncounterDeck
          shows <- shuffleM $ filter (cardMatch $ CardWithTrait Trait.Show) deck
          cornered <- genEncounterCard Treacheries.mojo39017
          for_ (headMay shows) $ putEncounterCardIntoPlay first
          createDeck showDeck (drop 1 shows <> [cornered])
      pure e
    MainSchemeMessage sid (MainSchemePlaceThreat n) | sid == mainSchemeId a, toCardCode a == "39002b" -> do
      threshold <- getThreshold a
      if mainSchemeThreat a + n >= threshold
        then do
          players <- fromIntegral <$> getPlayerCount
          rateChampion (2 * players)
          pure $ MojoOpeningMainScheme a {mainSchemeThreat = 0}
        else MojoOpeningMainScheme <$> runMessage msg a
    _ -> MojoOpeningMainScheme <$> runMessage msg a
