module Marvel.MainScheme.MainSchemes.AgeOfApocalypseUnus where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Difficulty
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Phase
import Marvel.Query
import Marvel.SideScheme.SideSchemes.AgeOfApocalypseInfinites (placeGenePoolThreat)

newtype AoaUnusMainScheme = AoaUnusMainScheme (Attrs MainScheme)
  deriving anyclass IsMainScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

huntingGeneTraitors :: MainSchemeCard AoaUnusMainScheme
huntingGeneTraitors = mainScheme AoaUnusMainScheme Cards.huntingGeneTraitors (PerPlayer 11) (Static 0) (PerPlayer 1)

instance RunMessage AoaUnusMainScheme where
  runMessage msg e@(AoaUnusMainScheme a) = runQueueT $ case msg of
    SetupMainScheme -> do
      difficulty <- getDifficulty
      when (difficulty == Expert) $ getPlayers >>= traverse_ dealEncounterCard
      first <- selectJust FirstPlayer
      chooseOnePrompted first "$label.aoaGenePoolDifficulty"
        [ labeled "$label.aoaGenePoolNoExtra" (pure ())
        , labeled "$label.aoaGenePoolExtraOne" (placeGenePoolThreat 1)
        , labeled "$label.aoaGenePoolExtraTwo" (placeGenePoolThreat 2)
        , labeled "$label.aoaGenePoolExtraThree" (placeGenePoolThreat 3)
        ]
      pure e
    AdvanceMainScheme sid | sid == mainSchemeId a -> e <$ gameOver Lost
    VillainPhaseStep StepEnemiesActivate -> e <$ placeGenePoolThreat 1
    _ -> AoaUnusMainScheme <$> runMessage msg a
