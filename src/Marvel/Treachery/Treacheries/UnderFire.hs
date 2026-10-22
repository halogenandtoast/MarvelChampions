module Marvel.Treachery.Treacheries.UnderFire (
  underFire,
  UnderFire (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

underFire :: TreacheryCard UnderFire
underFire = treachery UnderFire Cards.underFire

newtype UnderFire = UnderFire (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage UnderFire where
  runMessage msg t@(UnderFire attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      revealTopOfEncounterDeck identityId
      pure t
    _ -> UnderFire <$> runMessage msg attrs
