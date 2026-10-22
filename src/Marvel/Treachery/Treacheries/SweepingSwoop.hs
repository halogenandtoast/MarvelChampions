module Marvel.Treachery.Treacheries.SweepingSwoop (
  sweepingSwoop,
  SweepingSwoop (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Query
import Marvel.Queue
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

sweepingSwoop :: TreacheryCard SweepingSwoop
sweepingSwoop = treachery SweepingSwoop Cards.sweepingSwoop

newtype SweepingSwoop = SweepingSwoop (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage SweepingSwoop where
  runMessage msg t@(SweepingSwoop attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        vultureInPlay <- selectAny $ MinionIs Cards.vulture
        push $ IdentityMessage identityId IdentityStunned
        pure . SweepingSwoop $ attrs & surgeL .~ vultureInPlay
      _ -> SweepingSwoop <$> runMessage msg attrs
    -- [star] Boost: if this activation deals damage to a friendly character, stun it
    Boost (attackDamages -> Just (_, character)) -> do
      push $ GiveStatus (characterRef character) StunnedStatus
      pure t
    _ -> SweepingSwoop <$> runMessage msg attrs
