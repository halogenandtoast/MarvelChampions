module Marvel.MainScheme.MainSchemes.AgeOfApocalypseDarkBeast where

import Marvel.Prelude
import Marvel.Hp ()
import Marvel.Stats ()
import Marvel.Modifier ()
import Marvel.Card
import Marvel.Entity
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Difficulty
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query
import Marvel.Ref
import Marvel.GameValue
import Marvel.Message.Lifted

newtype AoaDarkBeastMainScheme = AoaDarkBeastMainScheme (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

darkBeastsBogusJourney :: MainSchemeCard AoaDarkBeastMainScheme
darkBeastsBogusJourney = mainScheme AoaDarkBeastMainScheme Cards.darkBeastsBogusJourney (PerPlayer 10) (PerPlayer 1) (PerPlayer 1)

instance RunMessage AoaDarkBeastMainScheme where
  runMessage msg e@(AoaDarkBeastMainScheme attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      difficulty <- getDifficulty
      when (difficulty == Expert) do
        first <- selectJust FirstPlayer
        searchEverywhereAndRevealEncounterCard first (CardIs Attachments.highTechGoggles)
      pure e
    _ -> AoaDarkBeastMainScheme <$> runMessage msg attrs
