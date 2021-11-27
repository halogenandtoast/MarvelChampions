module Marvel.Treachery.Treacheries.SweepingSwoop
  ( sweepingSwoop
  , SweepingSwoop(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

sweepingSwoop :: TreacheryCard SweepingSwoop
sweepingSwoop = treachery SweepingSwoop Cards.sweepingSwoop

newtype SweepingSwoop = SweepingSwoop TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage SweepingSwoop where
  runMessage msg t@(SweepingSwoop attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery identityId -> do
          vultureInPlay <- selectAny $ MinionIs Cards.vulture
          pushAll
            $ IdentityMessage identityId IdentityStunned
            : [ Surge identityId | vultureInPlay ]
          pure t
        _ -> SweepingSwoop <$> runMessage msg attrs
    Boost msg' -> case msg' of
      AllyMessage aid (AllyDamaged _ _) -> do
        push $ AllyMessage aid AllyStunned
        pure t
      IdentityMessage aid (IdentityDamaged _ _) -> do
        push $ IdentityMessage aid IdentityStunned
        pure t
      _ -> SweepingSwoop <$> runMessage msg attrs
    _ -> SweepingSwoop <$> runMessage msg attrs
