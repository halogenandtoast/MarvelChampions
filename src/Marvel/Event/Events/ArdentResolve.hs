module Marvel.Event.Events.ArdentResolve (
  ardentResolve,
  ArdentResolve (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Matchers qualified as M
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

ardentResolve :: EventCard ArdentResolve
ardentResolve = event ArdentResolve Cards.ardentResolve

newtype ArdentResolve = ArdentResolve (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Ready a friendly character (one that is exhausted, RR Target), then draw 1 card. The
-- draw is a target of its own, so the card is played with no character to ready.
instance RunMessage ArdentResolve where
  runMessage msg e@(ArdentResolve attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      identities <- selectList M.ExhaustedIdentity
      allies <- selectList M.ExhaustedAlly
      chooseOne you $
        [targeting (IdentityRef p) (ready p) | p <- identities]
          <> [targeting (AllyRef a) (ready a) | a <- allies]
      drawCards you 1
      pure e
    _ -> ArdentResolve <$> runMessage msg attrs
