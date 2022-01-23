module Marvel.Event.Events.GetBehindMe
  ( getBehindMe
  , GetBehindMe(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window qualified as W

getBehindMe :: EventCard GetBehindMe
getBehindMe = event GetBehindMe Cards.getBehindMe

newtype GetBehindMe = GetBehindMe EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage GetBehindMe where
  runMessage msg e@(GetBehindMe attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ (Just (W.RevealTreachery tid W.RevealedFromEncounterDeck))
        -> do
          villainId <- selectJust ActiveVillain
          replaceMatchingMessage
              (const [VillainMessage villainId $ VillainAttacks identityId])
            $ \case
                TreacheryMessage tid' (RevealTreachery _) -> tid' == tid
                _ -> False
          pure e
      _ -> GetBehindMe <$> runMessage msg attrs
    _ -> GetBehindMe <$> runMessage msg attrs
