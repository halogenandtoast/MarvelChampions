module Marvel.Event.Events.SplitPersonality
  ( splitPersonality
  , SplitPersonality(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Side
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Source
import Marvel.Target

splitPersonality :: EventCard SplitPersonality
splitPersonality = event SplitPersonality Cards.splitPersonality

newtype SplitPersonality = SplitPersonality EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage SplitPersonality where
  runMessage msg e@(SplitPersonality attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        inHero <- identityMatches HeroIdentity identityId
        let newForm = if inHero then B else A
        pushAll $ map
          (IdentityMessage identityId)
          [ChangedToForm newForm, DrawToHandLimit]
        pure e
      _ -> SplitPersonality <$> runMessage msg attrs
    _ -> SplitPersonality <$> runMessage msg attrs
