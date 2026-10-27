module Marvel.Event.Events.TripleThreat (
  tripleThreat,
  TripleThreat (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Id (CardId (..))
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Matchers qualified as M
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

tripleThreat :: EventCard TripleThreat
tripleThreat = event TripleThreat Cards.tripleThreat

newtype TripleThreat = TripleThreat (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Ready up to 3 characters, one at a time until the player is done. Any character may
-- be chosen, but only an exhausted one is a valid target (RR Target), and villains and
-- minions never exhaust: the identities and allies exhausted. Then it is removed from
-- the game, not discarded.
instance RunMessage TripleThreat where
  runMessage msg e@(TripleThreat attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent you _ _ -> do
        cardSpecific attrs "ready" (you, 3 :: Natural)
        pure e
      ResolvedEvent -> do
        removeFromGame (CardIdRef . CardId . unEventId $ eventId attrs)
        pure e
      _ -> TripleThreat <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "ready" (fromJSON -> Success (you, n :: Natural)) -> do
      identities <- selectList M.ExhaustedIdentity
      allies <- selectList M.ExhaustedAlly
      let again = when (n > 1) $ cardSpecific attrs "ready" (you, n - 1)
      when (n > 0 && (notNull identities || notNull allies)) $
        chooseOne you $
          labeled "$label.done" (pure ())
            : [targeting (IdentityRef p) (ready p >> again) | p <- identities]
            <> [targeting (AllyRef a) (ready a >> again) | a <- allies]
      pure e
    _ -> TripleThreat <$> runMessage msg attrs
