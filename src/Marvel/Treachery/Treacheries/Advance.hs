module Marvel.Treachery.Treacheries.Advance (
  advance,
  Advance (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

advance :: TreacheryCard Advance
advance = treachery Advance Cards.advance

newtype Advance = Advance (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage Advance where
  runMessage msg t@(Advance attrs) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident ->
      case msg' of
        RevealTreachery you -> do
          currentVillainSchemes you
          pure t
        _ -> Advance <$> runMessage msg attrs
    _ -> Advance <$> runMessage msg attrs
