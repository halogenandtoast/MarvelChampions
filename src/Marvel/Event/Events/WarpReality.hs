module Marvel.Event.Events.WarpReality (
  warpReality,
  WarpReality (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

warpReality :: EventCard WarpReality
warpReality = event WarpReality Cards.warpReality

newtype WarpReality = WarpReality (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The revealed card is discarded instead of resolving: it does not enter play, and
-- nothing of it resolves, incite and surge included. Its boost icons are counted as the
-- event resolves, and as many encounter cards discarded.
instance RunMessage WarpReality where
  runMessage msg e@(WarpReality attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ (Just (W.EncounterCardRevealed _ _ card))) | ident == eventId attrs -> do
      cancelMatchingMessage \case
        RevealedEncounterCard _ card' -> card' == card
        _ -> False
      discardCard (EncounterCard card)
      countBoostIcons attrs () [card]
      pure e
    WithBoostIcons (isTarget attrs -> True) _ (sum . map snd -> n) -> do
      discardTopOfEncounterDeck n
      pure e
    _ -> WarpReality <$> runMessage msg attrs
