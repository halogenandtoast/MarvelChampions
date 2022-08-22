module Marvel.Event.Events.CrisisInterdiction
  ( crisisInterdiction
  , CrisisInterdiction(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Target

crisisInterdiction :: EventCard CrisisInterdiction
crisisInterdiction = event CrisisInterdiction Cards.crisisInterdiction

newtype CrisisInterdiction = CrisisInterdiction EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage CrisisInterdiction where
  runMessage msg e@(CrisisInterdiction attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent _identityId _ _ -> do
        pure e
      _ -> CrisisInterdiction <$> runMessage msg attrs
    _ -> CrisisInterdiction <$> runMessage msg attrs
