module Marvel.MainScheme.MainSchemes.SmMysterio where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Ref

newtype SmMysterioMainScheme = SmMysterioMainScheme (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smMazeOfMirrors :: MainSchemeCard SmMysterioMainScheme
smMazeOfMirrors = mainScheme SmMysterioMainScheme Cards.smMazeOfMirrors (PerPlayer 8) (PerPlayer 2) (PerPlayer 1)

smEdgeOfReality :: MainSchemeCard SmMysterioMainScheme
smEdgeOfReality = mainScheme SmMysterioMainScheme Cards.smEdgeOfReality (PerPlayer 9) (PerPlayer 3) (PerPlayer 1)

instance RunMessage SmMysterioMainScheme where
  runMessage msg s@(SmMysterioMainScheme attrs) = runQueueT $ case msg of
    SetupMainScheme | toCardCode attrs == "27087" -> do
      players <- getPlayers
      cards <- filter (cardMatch $ CardIs Minions.smShiftingApparition) <$> getEncounterDeck
      for_ (zip players cards) (uncurry putEncounterCardIntoPlay)
      shuffleEncounterDeck
      pure s
    MainSchemeMessage sid RevealMainScheme | sid == mainSchemeId attrs, toCardCode attrs == "27088" -> do
      players <- getPlayers
      for_ players (`shuffleTopEncounterCardsIntoPlayerDeck` 2)
      pure s
    AdvanceMainScheme sid | sid == mainSchemeId attrs -> do
      if toCardCode attrs == "27087"
        then nextMainScheme sid
        else gameOver Lost
      pure s
    _ -> SmMysterioMainScheme <$> runMessage msg attrs
