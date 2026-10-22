module Marvel.MainScheme.MainSchemes.CountdownToOblivion where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted

countdownToOblivion :: MainSchemeCard CountdownToOblivion
countdownToOblivion =
  mainScheme CountdownToOblivion Cards.countdownToOblivion (PerPlayer 5) (Static 0) (PerPlayer 1)

newtype CountdownToOblivion = CountdownToOblivion (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage CountdownToOblivion where
  runMessage msg ms@(CountdownToOblivion attrs) = runQueueT $ case msg of
    MainSchemeMessage ident msg' | ident == mainSchemeId attrs -> case msg' of
      -- 3A: each player puts the top card of their deck into play as a Drone
      RevealMainScheme -> do
        players <- getPlayers
        for_ players putTopOfDeckIntoPlayAsDrone
        pure ms
      -- threat cannot be removed from this scheme
      MainSchemeThwarted _ _ -> pure ms
      _ -> CountdownToOblivion <$> runMessage msg attrs
    AdvanceMainScheme -> ms <$ gameOver Lost
    _ -> CountdownToOblivion <$> runMessage msg attrs
