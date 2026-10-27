module Marvel.Treachery.Treacheries.Waylay (
  waylay,
  Waylay (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

waylay :: TreacheryCard Waylay
waylay = treachery Waylay Cards.waylay

newtype Waylay = Waylay (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: stun and confuse Gamora. If she already was either, this card gains
-- surge.
instance RunMessage Waylay where
  runMessage msg t@(Waylay attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      gamoras <- selectList (IdentityWithTitle "Gamora")
      for_ gamoras \gamora -> do
        stunned <- identityMatches StunnedIdentity gamora
        confused <- identityMatches ConfusedIdentity gamora
        stunIdentity gamora
        confuseIdentity gamora
        when (stunned || confused) $ gainSurge attrs
      pure t
    _ -> Waylay <$> runMessage msg attrs
