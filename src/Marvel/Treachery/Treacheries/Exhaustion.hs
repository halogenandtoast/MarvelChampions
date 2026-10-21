module Marvel.Treachery.Treacheries.Exhaustion (
  exhaustion,
  Exhaustion (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Queue
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

exhaustion :: TreacheryCard Exhaustion
exhaustion = treachery Exhaustion Cards.exhaustion

newtype Exhaustion = Exhaustion (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage Exhaustion where
  runMessage msg t@(Exhaustion attrs) = case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      push $ IdentityMessage identityId ExhaustedIdentity
      pure t
    _ -> Exhaustion <$> runMessage msg attrs
