module Marvel.Treachery.Treacheries.CosmicInquisition (
  cosmicInquisition,
  CosmicInquisition (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

-- | Incite 2 (its keyword).
cosmicInquisition :: TreacheryCard CosmicInquisition
cosmicInquisition = treachery CosmicInquisition Cards.cosmicInquisition

newtype CosmicInquisition = CosmicInquisition (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: If the Universal Church of Truth side scheme is in play, discard the
-- top 10 cards of your deck. Otherwise, search the encounter deck, discard pile, and
-- set-aside area for Universal Church of Truth and reveal it. Shuffle the encounter deck.
instance RunMessage CosmicInquisition where
  runMessage msg t@(CosmicInquisition attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      let church = SideSchemes.universalChurchOfTruth
      inPlay <- inAnyArea $ selectAny (SideSchemeIs church)
      aside <- filter (cardMatch (CardIs church)) <$> getSetAsideEncounterCards
      if
        | inPlay -> discardTopOfDeck you 10
        | card : _ <- aside -> revealEncounterCard you card
        | otherwise -> searchAndRevealEncounterCard you (CardIs church)
      -- its own sentence: whichever way it went
      shuffleEncounterDeck
      pure t
    _ -> CosmicInquisition <$> runMessage msg attrs
