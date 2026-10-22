module Marvel.Treachery.Treacheries.ManipulatedTimestream (
  manipulatedTimestream,
  ManipulatedTimestream (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

manipulatedTimestream :: TreacheryCard ManipulatedTimestream
manipulatedTimestream = treachery ManipulatedTimestream Cards.manipulatedTimestream

newtype ManipulatedTimestream = ManipulatedTimestream (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: discard each event from your hand. If no events are discarded this
-- way, this card gains surge.
instance RunMessage ManipulatedTimestream where
  runMessage msg t@(ManipulatedTimestream attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      events <- filter (cardMatch $ CardWithType EventType) <$> projectMap PlayerIdentityHand unHand you
      if null events
        then pure . ManipulatedTimestream $ attrs & surgeL .~ True
        else t <$ for_ events discardCard
    _ -> ManipulatedTimestream <$> runMessage msg attrs
