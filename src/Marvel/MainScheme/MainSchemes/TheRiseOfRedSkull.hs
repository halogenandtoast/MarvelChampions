module Marvel.MainScheme.MainSchemes.TheRiseOfRedSkull where

import Marvel.Prelude

import Data.List (partition)

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemes

theRiseOfRedSkull :: MainSchemeCard TheRiseOfRedSkull
theRiseOfRedSkull =
  mainScheme TheRiseOfRedSkull Cards.theRiseOfRedSkull (PerPlayer 8) (Static 0) (PerPlayer 1)

newtype TheRiseOfRedSkull = TheRiseOfRedSkull (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | The top card of the side-scheme deck is revealed and put into play.
revealTopSideScheme :: (ReverseQueue m) => m ()
revealTopSideScheme = do
  first <- selectJust FirstPlayer
  fromDeck SideSchemeDeckKey (drawAndRevealEncounterCard first)

instance RunMessage TheRiseOfRedSkull where
  runMessage msg ms@(TheRiseOfRedSkull attrs) = runQueueT $ case msg of
    -- 1A (errata): put The Red House into play, shuffle every other encounter side
    -- scheme into the side-scheme deck, and set The Sleeper aside. The players' nemesis
    -- side schemes are set aside with their sets, not in the encounter deck.
    SetupMainScheme -> do
      Pile deck discard <- getPile EncounterDeckKey
      first <- selectJust FirstPlayer
      let cards = deck <> discard
          sideSchemes = filter (\c -> cdCardType (getCardDef c) == SideSchemeType) cards
          (redHouse, others) = partition (cardMatch (CardIs SideSchemes.theRedHouse)) sideSchemes
      for_ redHouse (putEncounterCardIntoPlay first)
      createDeck SideSchemeDeckKey others
      setAside [EncounterCard c | c <- cards, cardMatch (CardIs Minions.theSleeper) c]
      pure ms
    -- Forced Response: after step one of the villain phase
    AccelerateMainScheme -> do
      ms' <- TheRiseOfRedSkull <$> runMessage msg attrs
      revealTopSideScheme
      pure ms'
    AdvanceMainScheme -> ms <$ nextMainScheme
    _ -> TheRiseOfRedSkull <$> runMessage msg attrs
