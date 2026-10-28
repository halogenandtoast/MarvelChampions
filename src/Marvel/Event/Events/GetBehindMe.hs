module Marvel.Event.Events.GetBehindMe (
  getBehindMe,
  GetBehindMe (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

getBehindMe :: EventCard GetBehindMe
getBehindMe = event GetBehindMe Cards.getBehindMe

newtype GetBehindMe = GetBehindMe (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GetBehindMe where
  runMessage msg e@(GetBehindMe attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ (Just (W.RevealTreachery tid W.RevealedFromEncounterDeck)) -> do
        -- with no villain in play (as between one Kang and the next), nothing
        withTheVillain identityId \villainId -> do
          cancelWhenRevealed tid
          enemyAttacks villainId identityId
        pure e
      _ -> GetBehindMe <$> runMessage msg attrs
    _ -> GetBehindMe <$> runMessage msg attrs
