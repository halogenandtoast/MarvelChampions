module Marvel.MainScheme.MainSchemes.Mojo where

import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Entity
import Marvel.EncounterSet qualified as Set
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Environment.Cards qualified as Environments
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted
import Marvel.Matchers
import Marvel.Query
import Marvel.Ref

newtype MojoMainScheme = MojoMainScheme (Attrs MainScheme)
  deriving anyclass IsMainScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39025b :: MainSchemeCard MojoMainScheme
mojo39025b = mainScheme MojoMainScheme Cards.mojo39025b (PerPlayer 25) (PerPlayer 10) (PerPlayer 1)

instance RunMessage MojoMainScheme where
  runMessage msg e@(MojoMainScheme a) = runQueueT $ case msg of
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      putEncounterCardIntoPlay first =<< genEncounterCard Environments.mojo39026a
      pure e
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId a -> do
      n <- getPlayerCount
      cardSpecific GameRef "mojoSelectGenres" (fromIntegral n + 1 :: Natural, [] :: [Set.EncounterSet])
      pure e
    _ -> MojoMainScheme <$> runMessage msg a
