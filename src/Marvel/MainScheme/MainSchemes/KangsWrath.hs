module Marvel.MainScheme.MainSchemes.KangsWrath where

import Marvel.Prelude

import Marvel.Card
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Villain.Cards qualified as Villains

kangsWrath :: MainSchemeCard KangsWrath
kangsWrath = mainScheme KangsWrath Cards.kangsWrath (PerPlayer 10) (Static 0) (PerPlayer 1)

newtype KangsWrath = KangsWrath (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | The facedown Kang's Dominions under stage 4A, kept by the scenario as a deck.
kangsDominionDeck :: DeckKey
kangsDominionDeck = NamedDeckKey "kangsDominion"

-- | 4A When Revealed: reveal Kang (III) and add him to the game area; reveal each
-- facedown Kang's Dominion under this stage. Nothing on an earlier Kang carries over
-- (TOAFK insert p.7). 4B When Revealed: each player searches the encounter deck, discard
-- pile and set-aside area for their nemesis minion and puts it into play engaged with
-- them; shuffle the encounter deck. If this stage is completed, the players lose.
instance RunMessage KangsWrath where
  runMessage msg ms@(KangsWrath attrs) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      expert <- (== Expert) <$> getDifficulty
      addVillain (toCardCode $ if expert then Villains.kang3Expert else Villains.kang3)
      first <- selectJust FirstPlayer
      Pile under _ <- getPile kangsDominionDeck
      for_ under (revealEncounterCard first)
      removeDeck kangsDominionDeck
      players <- getPlayers
      for_ players \p -> traverse_ (putEncounterCardIntoPlay p) =<< getNemesisMinionsOutOfPlay p
      shuffleEncounterDeck
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> KangsWrath <$> runMessage msg attrs
