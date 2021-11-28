module Marvel.Event.Events.FirstAid
  ( firstAid
  , FirstAid(..)
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
import Marvel.Question
import Marvel.Source
import Marvel.Target

firstAid :: EventCard FirstAid
firstAid = event FirstAid Cards.firstAid

newtype FirstAid = FirstAid EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage FirstAid where
  runMessage msg e@(FirstAid attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        characters <- selectList CharacterWithAnyDamage
        chooseOne identityId
          $ map (\c -> TargetLabel (CharacterTarget c) [Heal c 2]) characters
        pure e
      _ -> FirstAid <$> runMessage msg attrs
    _ -> FirstAid <$> runMessage msg attrs
