module Marvel.MainScheme.MainSchemes.KangsArrival where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted
import Marvel.SideScheme.Cards qualified as SideSchemes

kangsArrival :: MainSchemeCard KangsArrival
kangsArrival = mainScheme KangsArrival Cards.kangsArrival (PerPlayer 7) (Static 0) (PerPlayer 1)

newtype KangsArrival = KangsArrival (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1A Setup: set each Kang (II), Kang (III) and Kang's Dominion aside (the villains are
-- the scenario's, entering play with stages 3A and 4A); remove each player's obligation
-- cards from the game; shuffle the encounter deck. 1B When Revealed: deal each player an
-- encounter card. If this stage is completed, the players lose.
instance RunMessage KangsArrival where
  runMessage msg ms@(KangsArrival attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      Pile deck discard <- getPile EncounterDeckKey
      setAside [EncounterCard c | c <- deck <> discard, cardMatch (CardIs SideSchemes.kangsDominion) c]
      removePlayerObligationsFromGame
      shuffleEncounterDeck
      pure ms
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      traverse_ dealEncounterCard =<< getPlayers
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> KangsArrival <$> runMessage msg attrs
